{-|
Module:      Tesla.Auth
Description: Tesla Authentication structures.

Authentication related data structures.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Auth (AuthInfo(..),
                   clientID, clientSecret, email, password, bearerToken,
                   fromToken,
                   AuthResponse(..), access_token, expires_in, refresh_token,
                   HasTeslaAuth(..)
                  ) where

import           Control.Lens
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

makeLenses ''AuthInfo

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

makeLenses ''AuthResponse

-- | A Monad may have a 'HasTeslaAuth' instance to indicate it knows
-- how to authenticate against the Tesla service.
class Monad m => HasTeslaAuth m where
  teslaAuth :: m AuthInfo

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts
