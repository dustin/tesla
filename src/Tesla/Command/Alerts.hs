{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Alerts (
  honkHorn, flashLights
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Command

honkHorn :: MonadIO m => Car m CommandResponse
honkHorn = runCmd' "honk_horn"

flashLights :: MonadIO m => Car m CommandResponse
flashLights = runCmd' "flash_lights"
