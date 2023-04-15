{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Software (
  scheduleUpdate, cancelUpdate
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Car.Command

-- | Schedule a software update in this many seconds.
scheduleUpdate :: MonadIO m => Int -> Car m CommandResponse
scheduleUpdate secs = runCmd "schedule_software_update" ["offset_sec" .= secs]

-- | Cancel a scheduled software update.
mkCommand "cancelUpdate" "cancel_software_update"

