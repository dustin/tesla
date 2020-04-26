{-|
Module:      Tesla
Description: Tesla API implementation.

'Tesla' is intended to provide access to all known Tesla APIs as
documented at https://tesla-api.timdorr.com/
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tesla
    ( authenticate, refreshAuth, AuthResponse(..),
      Product(..), carName, carID, solarID, _ProductCar, _ProductSolar, _ProductPowerWall,
      AuthInfo(..),
      vehicles, products, decodeProducts,
      solarIDs,
      fromToken, authOpts, baseURL
    ) where


import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), Options (..), Value (..), defaultOptions, fieldLabelModifier,
                                         genericParseJSON)
import           Data.Aeson.Lens        (key, _Array, _String)
import qualified Data.ByteString.Char8  as BC
import           Data.Foldable          (asum)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (FormParam (..), Options, Response, asJSON, defaults, getWith, header, postWith,
                                         responseBody)

baseURL :: String
baseURL = "https://owner-api.teslamotors.com/"
authURL :: String
authURL = baseURL <> "oauth/token"
authRefreshURL :: String
authRefreshURL = baseURL <> "oauth/token"
productsURL :: String
productsURL = baseURL <> "api/1/products"

userAgent :: BC.ByteString
userAgent = "github.com/dustin/tesla 0.1"

defOpts :: Network.Wreq.Options
defOpts = defaults & header "User-Agent" .~ [userAgent]

-- | An Authentication request.
data AuthInfo = AuthInfo {
  _clientID       :: String
  , _clientSecret :: String
  , _email        :: String
  , _password     :: String
  , _bearerToken  :: String
  } deriving(Show)

-- | Get an AuthInfo instance from a bearer token.
fromToken :: String -> AuthInfo
fromToken t = AuthInfo{_bearerToken=t, _clientID="", _clientSecret="", _email="", _password=""}

jsonOpts :: Data.Aeson.Options
jsonOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

-- | An Authentication response.
data AuthResponse = AuthResponse {
  _access_token    :: String
  , _expires_in    :: Int
  , _refresh_token :: String
  } deriving(Generic, Show)

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts

-- | Authenticate to the Tesla service.
authenticate :: AuthInfo -> IO AuthResponse
authenticate AuthInfo{..} = do
  r <- asJSON =<< postWith defOpts authURL ["grant_type" := ("password" :: String),
                                            "client_id" := _clientID,
                                            "client_secret" := _clientSecret,
                                            "email" := _email,
                                            "password" := _password] :: IO (Response AuthResponse)
  pure $ r ^. responseBody

-- | Refresh authentication credentials using a refresh token.
refreshAuth :: AuthInfo -> AuthResponse -> IO AuthResponse
refreshAuth AuthInfo{..} AuthResponse{..} = do
  r <- asJSON =<< postWith defOpts authRefreshURL ["grant_type" := ("refresh_token" :: String),
                                                   "client_id" := _clientID,
                                                   "client_secret" := _clientSecret,
                                                   "refresh_token" := _refresh_token] :: IO (Response AuthResponse)
  pure $ r ^. responseBody


-- | Get a set of wreq options from an 'AuthInfo'.
authOpts :: AuthInfo -> Network.Wreq.Options
authOpts AuthInfo{..} = defOpts & header "Authorization" .~ ["Bearer " <> BC.pack _bearerToken]

-- | Tesla Product Types.
data Product = ProductCar { _carName :: Text, _carID :: Text }
             | ProductSolar { _solarID :: Text }
             | ProductPowerWall deriving (Show, Read, Eq)

makePrisms ''Product
makeLenses ''Product

decodeProducts :: Value -> [Product]
decodeProducts = catMaybes . toListOf (key "response" . _Array . folded . to prod)
  where
    prod o = asum [ prodCar, prodSolar, Nothing ]
      where
        prodCar = ProductCar <$> (o ^? key "display_name" . _String) <*> (o ^? key "id_s" . _String)
        prodSolar = ProductSolar <$> (o ^? key "id" . _String)

-- | Get all products associated with this account.
products :: MonadIO m => AuthInfo -> m [Product]
products ai = decodeProducts . view responseBody <$> liftIO (asJSON =<< getWith (authOpts ai) productsURL)

-- | Get a mapping of vehicle name to vehicle ID.
vehicles :: MonadIO m => AuthInfo -> m (Map Text Text)
vehicles = fmap (Map.fromList . toListOf (folded . _ProductCar)) . products

-- | Get a list of Solar ID installations.
solarIDs :: MonadIO m => AuthInfo -> m [Text]
solarIDs = fmap (toListOf $ folded . solarID) . products
