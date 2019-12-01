{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Homelink (trigger) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))
import           Tesla.Command

-- | Trigger nearby homelink with the given (lat,lon)
trigger :: MonadIO m => (Double, Double) -> Car m CommandResponse
trigger (lat,lon) = runCmd "flash_lights" ["lat" := lat, "lon" := lon]
