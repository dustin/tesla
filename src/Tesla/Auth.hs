{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tesla.Auth where

import           Data.Aeson             (FromJSON (..), Options, defaultOptions, fieldLabelModifier, genericParseJSON)
import           Generics.Deriving.Base (Generic)

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

-- | A Monad may have a 'HasTeslaAuth' instance to indicate it knows
-- how to authenticate against the Tesla service.
class Monad m => HasTeslaAuth m where
  teslaAuth :: m AuthInfo


instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts
