{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Software (
  schedule, cancel
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Command

-- | Schedule a software update in this many seconds.
schedule :: MonadIO m => Int -> Car m CommandResponse
schedule secs = runCmd "schedule_software_update" ["offset_sec" := secs]

-- | Cancel a scheduled software update.
cancel :: MonadIO m => Car m CommandResponse
cancel = runCmd' "cancel_software_update"
