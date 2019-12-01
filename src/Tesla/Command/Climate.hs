{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Climate (
  hvacOn, hvacOff, heatSeat, Seat(..),
  setTemps, wheelHeater, wheelHeaterOff, wheelHeaterOn
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Char              (toLower)
import           Network.Wreq           (FormParam (..))

import           Tesla.Command

-- | Turn HVAC on.
hvacOn :: MonadIO m => Car m CommandResponse
hvacOn = runCmd' "auto_conditioning_start"

-- | Turn HVAC off.
hvacOff :: MonadIO m => Car m CommandResponse
hvacOff = runCmd' "auto_conditioning_stop"

-- | Turn on the steering wheel heater
wheelHeater :: MonadIO m => Bool -> Car m CommandResponse
wheelHeater on = runCmd "remote_steering_wheel_heater_request" ["on" := map toLower (show on)]

wheelHeaterOn :: MonadIO m => Car m CommandResponse
wheelHeaterOn = wheelHeater True

wheelHeaterOff :: MonadIO m => Car m CommandResponse
wheelHeaterOff = wheelHeater False

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

