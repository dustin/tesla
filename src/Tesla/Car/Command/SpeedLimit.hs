{-# LANGUAGE OverloadedStrings #-}

module Tesla.Car.Command.SpeedLimit (
  speedLimit, activateSpeedLimit, deactivateSpeedLimit, clearSpeedLimitPIN
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))
import           Tesla.Car.Command

speedLimit :: MonadIO m => Int -> Car m CommandResponse
speedLimit to = runCmd "speed_limit_set_limit" ["limit_mph" := to ]

speedo :: MonadIO m => String -> Int -> Car m CommandResponse
speedo c pin = runCmd c ["pin" := pin ]

activateSpeedLimit :: MonadIO m => Int -> Car m CommandResponse
activateSpeedLimit = speedo "speed_limit_activate"

deactivateSpeedLimit :: MonadIO m => Int -> Car m CommandResponse
deactivateSpeedLimit = speedo "speed_limit_deactivate"

clearSpeedLimitPIN :: MonadIO m => Int -> Car m CommandResponse
clearSpeedLimitPIN = speedo "speed_limit_clear_pin"
