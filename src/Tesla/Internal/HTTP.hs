{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module Tesla.Internal.HTTP where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..))
import qualified Data.ByteString.Char8  as BC
import           Network.Wreq           (Options, asJSON, defaults, get, getWith, header, postWith, responseBody)
import           Network.Wreq.Types     (Postable)

import           Tesla.Auth

userAgent :: BC.ByteString
userAgent = "github.com/dustin/tesla 0.1"

defOpts :: Options
defOpts = defaults & header "User-Agent" .~ [userAgent]

-- | Get a set of wreq options from an 'AuthInfo'.
authOpts :: AuthInfo -> Options
authOpts AuthInfo{..} = defOpts & header "Authorization" .~ ["Bearer " <> BC.pack _bearerToken]

jget :: (FromJSON j, MonadIO m) => String -> m j
jget u = view responseBody <$> liftIO (asJSON =<< get u)

jgetWith :: (FromJSON j, MonadIO m) => Options -> String -> m j
jgetWith opts u = view responseBody <$> liftIO (asJSON =<< getWith opts u)

jgetAuth :: (HasTeslaAuth m, FromJSON j, MonadIO m) => String -> m j
jgetAuth u = teslaAuth >>= \a -> view responseBody <$> liftIO (asJSON =<< getWith (authOpts a) u)

jpostWith :: (FromJSON j, Postable a, MonadIO m) => Options -> String -> a -> m j
jpostWith opts u v = view responseBody <$> liftIO (postWith opts u v >>= asJSON)

jpostAuth :: (HasTeslaAuth m, FromJSON j, Postable a, MonadIO m) => String -> a -> m j
jpostAuth u v = teslaAuth >>= \a -> jpostWith (authOpts a & header "Content-Type" .~ ["application/json"]) u v
