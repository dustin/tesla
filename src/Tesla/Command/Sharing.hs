{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Sharing (share) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson
import           Data.Text              (Text)
import           Data.Time.Clock.POSIX  (getCurrentTime, utcTimeToPOSIXSeconds)

import           Tesla.Command

share :: MonadIO m => Text -> Car m CommandResponse
share to = do
  now <- fst . properFraction . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  runCmd "share" $ object [
    "type" .= ("share_ext_content_raw" :: Text),
    "value" .= object [ "android.intent.extra.TEXT" .= to ],
    "locale" .= ("en-US" :: Text),
    "timestamp_ms" .= (now * (1000::Int))
    ]
