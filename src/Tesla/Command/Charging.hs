{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Charging (
  startCharging, stopCharging, setLimit, openChargePort, closeChargePort
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Command

startCharging :: MonadIO m => Car m CommandResponse
startCharging = runCmd' "charge_start"

stopCharging :: MonadIO m => Car m CommandResponse
stopCharging = runCmd' "charge_stop"

setLimit :: MonadIO m => Int -> Car m CommandResponse
setLimit to = runCmd "set_charge_limit" ["percent" := to ]

openChargePort :: MonadIO m => Car m CommandResponse
openChargePort = runCmd' "charge_port_door_open"

closeChargePort :: MonadIO m => Car m CommandResponse
closeChargePort = runCmd' "charge_port_door_close"
