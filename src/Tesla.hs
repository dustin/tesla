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
      Product(..), vehicleName, vehicleID, energyID, _ProductVehicle, _ProductEnergy, _ProductPowerWall,
      VehicleID, vehicles, products,
      EnergyID, energyIDs,
      fromToken, authOpts, baseURL,
      decodeProducts
    ) where


import           Control.Lens
import           Control.Monad              (when)
import           Control.Monad.Catch        (SomeException)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Retry              (defaultLogMsg, exponentialBackoff, limitRetries, logRetries, recovering)
import           Crypto.Hash                (SHA256 (..), hashWith)
import           Data.Aeson                 (Value (..), encode)
import           Data.Aeson.Lens            (_Array, _Integer, _String, key)
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable              (asum)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.Wreq               (FormParam (..), Options, asJSON, checkResponse, defaults, header,
                                             hrRedirects, params, redirects, responseBody, responseHeader)
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
authenticate ai@AuthInfo{..} = recovering policy [retryOnAnyStatus] $ \_ -> do
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
  r2 <- Sess.customHistoriedPayloadMethodWith "POST" (fopts
                                                       & params .~ gparams
                                                       & redirects .~ 3
                                                       & checkResponse ?~ (\_ _ -> pure ())
                                                     ) sess authURL form'
  -- Extract the "code" from the URL we were redirected to... we can't actually follow the redirect :/
  let (Just code) = r2 ^? hrRedirects . folded . _2 . responseHeader "Location" . to (xcode . BC.unpack)
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
    xcode u = head . mapMaybe (\s -> let [k,v] = T.splitOn "=" s in if k == "code" then Just v else Nothing) $ T.splitOn "&" (T.splitOn "?" (T.pack u) !! 1)

translateCreds :: AuthInfo -> AuthResponse -> IO AuthResponse
translateCreds ai@AuthInfo{..} AuthResponse{..} = do
  -- 4. And we finally get the useful credentials by exchanging the temporary credentials.
  let jreq2 = encode $ Object (mempty
                               & at "grant_type" ?~ "urn:ietf:params:oauth:grant-type:jwt-bearer"
                               & at "client_id" ?~ String (T.pack _clientID)
                              )

  ar <- jpostWith (jOpts & header "Authorization" .~ ["bearer " <> BC.pack _access_token]) authTokenURL jreq2
  pure (ar & refresh_token .~ _refresh_token) -- replace the refresh token with the one from step 3


-- | Refresh authentication credentials using a refresh token.
refreshAuth :: AuthInfo -> AuthResponse -> IO AuthResponse
refreshAuth ai@AuthInfo{..} AuthResponse{..} = do
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
aOpts = defaults
        & header "Accept" .~ ["*/*"]
        & header "User-Agent" .~ [ua]
        & header "x-tesla-user-agent" .~ [userAgent]
        & header "X-Requested-With" .~ ["com.teslamotors.tesla"]
  where ua = "Mozilla/5.0 (Linux; Android 10; Pixel 3 Build/QQ2A.200305.002; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/85.0.4183.81 Mobile Safari/537.36"

-- | A VehicleID.
type VehicleID = Text

-- | An energy site ID.
type EnergyID = Integer

-- | Tesla Product Types.
data Product = ProductVehicle { _vehicleName :: Text, _vehicleID :: VehicleID }
             | ProductEnergy { _energyID :: EnergyID }
             | ProductPowerWall deriving (Show, Read, Eq)

makePrisms ''Product
makeLenses ''Product

decodeProducts :: Value -> [Product]
decodeProducts = catMaybes . toListOf (key "response" . _Array . folded . to prod)
  where
    prod o = asum [ prodCar, prodSolar, Nothing ]
      where
        prodCar = ProductVehicle <$> (o ^? key "display_name" . _String) <*> (o ^? key "id_s" . _String)
        prodSolar = ProductEnergy <$> (o ^? key "energy_site_id" . _Integer)

-- | Get all products associated with this account.
products :: MonadIO m => AuthInfo -> m [Product]
products ai = decodeProducts <$> jgetWith (authOpts ai) productsURL

-- | Get a mapping of vehicle name to vehicle ID.
vehicles :: [Product] -> Map Text Text
vehicles = Map.fromList . toListOf (folded . _ProductVehicle)

-- | Get a list of Solar ID installations.
energyIDs :: [Product] -> [EnergyID]
energyIDs = toListOf (folded . energyID)
