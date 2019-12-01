{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Software (
  scheduleUpdate, cancelUpdate
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Command
import           Tesla.Command.TH

-- | Schedule a software update in this many seconds.
scheduleUpdate :: MonadIO m => Int -> Car m CommandResponse
scheduleUpdate secs = runCmd "schedule_software_update" ["offset_sec" := secs]

-- | Cancel a scheduled software update.
carCMD "cancelUpdate" "cancel_software_update"

