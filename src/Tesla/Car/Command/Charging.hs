{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Charging (
  startCharging, stopCharging, setLimit, openChargePort, closeChargePort,
  setAmps,
  scheduledChargingOff, scheduleCharging
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Car.Command

-- | Set the desired charge level (percent).
setLimit :: MonadIO m => Int -> Car m CommandResponse
setLimit to = runCmd "set_charge_limit" ["percent" .= to]

-- | Set the charge current.
setAmps :: MonadIO m => Int -> Car m CommandResponse
setAmps to = runCmd "set_charging_amps" ["charging_amps" .= to]

-- | Disable scheduled charging.
scheduledChargingOff :: MonadIO m => Car m CommandResponse
scheduledChargingOff = runCmd "set_scheduled_charging" [ "enable" .= False ]

-- | Schedule charging for the given number of minutes after midnight (local time).
scheduleCharging :: MonadIO m => Time -> Car m CommandResponse
scheduleCharging mins = runCmd "set_scheduled_charging" [ "enable" .= True, "time" .= mins ]

mkNamedCommands [("startCharging", "charge_start"),
                 ("stopCharging", "charge_stop"),
                 ("openChargePort", "charge_port_door_open"),
                 ("closeChargePort", "charge_port_door_close")]
