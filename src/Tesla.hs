{-|
Module:      Tesla
Description: Tesla API implementation.

'Tesla' is intended to provide access to all known Tesla APIs as
documented at https://www.teslaapi.io/
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tesla
    ( authenticate, refreshAuth, AuthResponse(..),
      Product(..), vehicleName, vehicleID, vehicleState,
      energyID, _ProductVehicle, _ProductEnergy, _ProductPowerwall,
      pwBatteryPower, pwCharged, pwEnergyLeft, pwID, pwName, pwTotal,
      VehicleID, vehicles, products, productsRaw,
      VehicleState(..), vsFromString,
      EnergyID, energyIDs,
      fromToken, authOpts, baseURL,
      decodeProducts
    ) where


import           Control.Exception          (catch)
import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.Catch        (SomeException)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Retry              (defaultLogMsg, exponentialBackoff, limitRetries, logRetries, recovering)
import           Crypto.Hash                (SHA256 (..), hashWith)
import           Data.Aeson                 (FromJSON, Value (..), encode)
import           Data.Aeson.Lens            (_Array, _Double, _Integer, _String, key)
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable              (asum)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Client        (HttpException (..), HttpExceptionContent (TooManyRedirects))
import           Network.Wreq               (FormParam (..), Options, asJSON, checkResponse, defaults, header,
                                             hrFinalResponse, params, redirects, responseBody, responseHeader)
import qualified Network.Wreq.Session       as Sess
import           System.Random
import           Text.HTML.TagSoup          (fromAttrib, isTagOpenName, parseTags)

import           Tesla.Auth
import           Tesla.Internal.HTTP

baseURL :: String
baseURL =  "https://owner-api.teslamotors.com/"
authURL :: String
authURL = "https://auth.tesla.com/oauth2/v3/authorize"
authTokenURL :: String
authTokenURL = "https://owner-api.teslamotors.com/oauth/token"
authRefreshURL :: String
authRefreshURL = "https://auth.tesla.com/oauth2/v3/token"
productsURL :: String
productsURL = baseURL <> "api/1/products"

-- | Authenticate to the Tesla service.
authenticate :: AuthInfo -> IO AuthResponse
authenticate ai = recovering policy [retryOnAnyStatus] $ \_ -> do
  sess <- Sess.newSession
  verifier <- BS.pack . take 86 . randoms <$> getStdGen
  state <- clean64 . BS.pack . take 16 . randoms <$> getStdGen
  authenticate' sess verifier state ai

  where
    policy = exponentialBackoff 2000000 <> limitRetries 9
    retryOnAnyStatus = logRetries retryOnAnyError reportError
    retryOnAnyError :: SomeException -> IO Bool
    retryOnAnyError _ = pure True
    reportError retriedOrCrashed err retryStatus = putStrLn $ defaultLogMsg retriedOrCrashed err retryStatus

clean64 :: BC.ByteString -> Text
clean64 = TE.decodeUtf8 . BS.reverse . BS.dropWhile (== 61) . BS.reverse . B64.encode

authenticate' :: Sess.Session -> BC.ByteString -> Text -> AuthInfo -> IO AuthResponse
authenticate' sess verifier state ai@AuthInfo{..} = do
  -- 1. First, grab the form.
  form <- formFields . BL.toStrict . view responseBody <$> Sess.getWith (aOpts & params .~ gparams) sess authURL
  -- There are required hidden fields -- if we didn't get them, we got the wrong http response
  when (null form) $ fail "tesla didn't return login form"

  -- 2. Now we post the form with all of our credentials.
  let form' = form <> ["identity" := _email, "credential" := _password]
  Just code <- fmap (xcode . BC.unpack) <$> findRedirect authURL (fopts
                                                                  & params .~ gparams
                                                                  & redirects .~ 0
                                                                  & checkResponse ?~ (\_ _ -> pure ())
                                                                 ) form'
  -- Extract the "code" from the URL we were redirected to... we can't actually follow the redirect :/
  let jreq = encode $ Object (mempty
                               & at "grant_type" ?~ "authorization_code"
                               & at "client_id" ?~ "ownerapi"
                               & at "code" ?~ String code
                               & at "code_verifier" ?~ String verifierHash
                               & at "redirect_uri" ?~ "https://auth.tesla.com/void/callback")

  -- 3. Posting that code and other junk back to the token URL gets us temporary credentials.
  ar <- view responseBody <$> (Sess.postWith jOpts sess authRefreshURL jreq >>= asJSON)
  translateCreds ai ar

  where
    verifierHash = clean64 . BS.pack . BA.unpack $ hashWith SHA256 verifier
    gparams = [("client_id", "ownerapi"),
                 ("code_challenge", verifierHash),
                 ("code_challenge_method", "S256"),
                 ("redirect_uri", "https://auth.tesla.com/void/callback"),
                 ("response_type", "code"),
                 ("scope", "openid email offline_access"),
                 ("state", state)]
    -- extract all the non-empty form fields from an HTML response
    formFields = map (\t -> fromAttrib "name" t := fromAttrib "value" t)
                 . filter (\t -> isTagOpenName "input" t && fromAttrib "value" t /= "")
                 . parseTags
    fopts = aOpts & header "content-type" .~ ["application/x-www-form-urlencoded"]
    xcode u = head [v | q <- tail (T.splitOn "?" (T.pack u)),
                        kv <- T.splitOn "&" q,
                        (k,v) <- paird (T.splitOn "=" kv),
                        k == "code"]
    paird [a,b] = [(a,b)]
    paird _     = []

    findRedirect u opts a = preview (_Just . responseHeader "Location") <$> (inBody `catch` inException)
      where
        inBody = preview hrFinalResponse <$> Sess.customHistoriedPayloadMethodWith "POST" opts sess u a
        inException (HttpExceptionRequest _ (TooManyRedirects (r:_))) = pure (Just r)


translateCreds :: AuthInfo -> AuthResponse -> IO AuthResponse
translateCreds AuthInfo{..} AuthResponse{..} = do
  -- 4. And we finally get the useful credentials by exchanging the temporary credentials.
  let jreq2 = encode $ Object (mempty
                               & at "grant_type" ?~ "urn:ietf:params:oauth:grant-type:jwt-bearer"
                               & at "client_id" ?~ String (T.pack _clientID)
                              )

  ar <- jpostWith (jOpts & header "Authorization" .~ ["bearer " <> BC.pack _access_token]) authTokenURL jreq2
  pure (ar & refresh_token .~ _refresh_token) -- replace the refresh token with the one from step 3


-- | Refresh authentication credentials using a refresh token.
refreshAuth :: AuthInfo -> AuthResponse -> IO AuthResponse
refreshAuth ai AuthResponse{..} = do
  ar <- jpostWith jOpts authRefreshURL (encode $ Object (mempty
                                                         & at "grant_type" ?~ "refresh_token"
                                                         & at "client_id" ?~ "ownerapi"
                                                         & at "refresh_token" ?~ String (T.pack _refresh_token)
                                                         & at "scope" ?~ "openid email offline_access"
                                                        ))
  translateCreds ai ar

jOpts :: Options
jOpts = aOpts & header "content-type" .~ ["application/json"]

aOpts :: Options
aOpts = defaults & header "Accept" .~ ["*/*"]

-- | A VehicleID.
type VehicleID = Text

-- | An energy site ID.
type EnergyID = Integer

-- | Possible states a vehicle may be in.
data VehicleState = VOnline | VOffline | VAsleep | VWaking | VUnknown
  deriving (Show, Read, Eq)

vsFromString :: Text -> VehicleState
vsFromString "online"  = VOnline
vsFromString "offline" = VOffline
vsFromString "asleep"  = VAsleep
vsFromString "waking"  = VWaking
vsFromString _         = VUnknown

-- | Tesla Product Types.
data Product = ProductVehicle { _vehicleName :: Text, _vehicleID :: VehicleID, _vehicleState :: VehicleState }
             | ProductEnergy { _energyID :: EnergyID }
             | ProductPowerwall { _pwID           :: EnergyID
                                , _pwBatteryPower :: Double
                                , _pwEnergyLeft   :: Double
                                , _pwCharged      :: Double
                                , _pwName         :: Text
                                , _pwTotal        :: Double }
             deriving (Show, Read, Eq)

makePrisms ''Product
makeLenses ''Product

-- | Decode a products response into a list of products.
decodeProducts :: Value -> [Product]
decodeProducts = catMaybes . toListOf (key "response" . _Array . folded . to prod)
  where
    prod o = asum [ prodCar, prodPowerwall, prodSolar, Nothing ]
      where
        prodCar = ProductVehicle
                  <$> (o ^? key "display_name" . _String)
                  <*> (o ^? key "id_s" . _String)
                  <*> (o ^? key "state" . _String . to vsFromString)
        prodPowerwall = ProductPowerwall
                        <$> (o ^? key "energy_site_id" . _Integer)
                        <*> (o ^? key "battery_power" . _Double)
                        <*> (o ^? key "energy_left" . _Double)
                        <*> (o ^? key "percentage_charged" . _Double)
                        <*> (o ^? key "site_name" . _String)
                        <*> (o ^? key "total_pack_energy" . _Double)
        prodSolar = ProductEnergy <$> (o ^? key "energy_site_id" . _Integer)

-- | productsRaw retrieves the complete response for products
productsRaw :: (FromJSON j, MonadIO m) => AuthInfo -> m j
productsRaw ai = jgetWith (authOpts ai) productsURL

-- | Get all products associated with this account.
products :: MonadIO m => AuthInfo -> m [Product]
products = fmap decodeProducts . productsRaw

-- | Get a mapping of vehicle name to vehicle ID.
vehicles :: [Product] -> Map Text Text
vehicles = Map.fromList . fmap (\(a,b,_) -> (a,b)) . toListOf (folded . _ProductVehicle)

-- | Get a list of Solar ID installations.
energyIDs :: [Product] -> [EnergyID]
energyIDs = toListOf (folded . energyID)
