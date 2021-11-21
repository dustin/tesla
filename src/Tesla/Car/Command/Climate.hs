{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Climate (
  hvacOn, hvacOff, heatSeat, Seat(..),
  setTemps, wheelHeater, wheelHeaterOff, wheelHeaterOn,
  maxDefrost,
  wakeUp,
  bioweaponMode
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Car.Command

-- | Turn on the steering wheel heater
wheelHeater :: MonadIO m => Bool -> Car m CommandResponse
wheelHeater on = runCmd "remote_steering_wheel_heater_request" ["on" := on]

wheelHeaterOn :: MonadIO m => Car m CommandResponse
wheelHeaterOn = wheelHeater True

wheelHeaterOff :: MonadIO m => Car m CommandResponse
wheelHeaterOff = wheelHeater False

-- | Turn on or off bioweapon defense mode.
--
-- If HVAC is off, turning on bioweapon defense mode will also turn on HVAC.
bioweaponMode :: MonadIO m => Bool -> Car m CommandResponse
bioweaponMode on = runCmd "set_bioweapon_mode" ["on" := on]

data Seat = DriverSeat | PassengerSeat | RearLeftSeat | RearCenterSeat | RearRightSeat

-- | Set heating levels for various seats.
heatSeat :: MonadIO m => Seat -> Int -> Car m CommandResponse
heatSeat seat level = runCmd "remote_seat_heater_request" ["heater" := seatNum seat, "level" := level]
  where
    seatNum :: Seat -> Int
    seatNum DriverSeat     = 0
    seatNum PassengerSeat  = 1
    seatNum RearLeftSeat   = 2
    seatNum RearCenterSeat = 4
    seatNum RearRightSeat  = 5

-- | Set the main HVAC temperatures.
setTemps :: MonadIO m => (Double, Double) -> Car m CommandResponse
setTemps (driver, passenger) = runCmd "set_temps" ["driver_temp" := driver, "passenger_temp" := passenger]

maxDefrost :: MonadIO m => Bool -> Car m CommandResponse
maxDefrost on = runCmd "set_preconditioning_max" ["on" := on]

mkNamedCommands [("hvacOn", "auto_conditioning_start"),
                 ("hvacOff", "auto_conditioning_stop"),
                 ("wakeUp", "wake_up")]

