{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Charging (
  startCharging, stopCharging, setLimit, openChargePort, closeChargePort,
  setAmps
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Car.Command

-- | Set the desired charge level (percent).
setLimit :: MonadIO m => Int -> Car m CommandResponse
setLimit to = runCmd "set_charge_limit" ["percent" := to ]

-- | Set the charge current.
setAmps :: MonadIO m => Int -> Car m CommandResponse
setAmps to = runCmd "set_charging_amps" ["charging_amps" := to]

mkNamedCommands [("startCharging", "charge_start"),
                 ("stopCharging", "charge_stop"),
                 ("openChargePort", "charge_port_door_open"),
                 ("closeChargePort", "charge_port_door_close")]
