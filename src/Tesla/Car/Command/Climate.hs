{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Climate (
  hvacOn, hvacOff, heatSeat, Seat(..),
  setTemps, wheelHeater, wheelHeaterOff, wheelHeaterOn,
  maxDefrost,
  wakeUp,
  bioweaponMode,
  Sometimes(..), OffPeakConfig(..), Preconditioning,
  scheduledDepartureOff, scheduleDeparture
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

scheduledDepartureOff :: MonadIO m => Car m CommandResponse
scheduledDepartureOff = runCmd "set_scheduled_departure" [ "enable" := False ]

-- | When configuring scheduled departure, preconditioning and
-- off-peak charging both have weekday only options.
data Sometimes = Never | Always | WeekdaysOnly

-- | Type alias to make 'scheduleDeparture' more readable.
type Preconditioning = Sometimes

-- | Configuration for off-peak charging for a schedule departure.
data OffPeakConfig = OffPeakConfig {
  _offPeakEnabled :: Sometimes,
  _offPeakEndTime :: Time
  }

-- | Schedule a departure.
--
-- For this to do anything useful, you need to specify at least one of
-- 'Preconditioning' and/or 'OffPeakConfig'.
scheduleDeparture :: MonadIO m => Time -> Preconditioning -> Maybe OffPeakConfig -> Car m CommandResponse
scheduleDeparture t p o = runCmd "set_scheduled_departure" (["enable" := True, "departure_time" := t] <> pp <> op o)
  where
    pp = s "preconditioning_enabled" "preconditioning_weekdays_only" p
    op Nothing  = opp (OffPeakConfig Never (Time 0))
    op (Just x) = opp x
    opp OffPeakConfig{..} = ("end_off_peak_time" := _offPeakEndTime) : s "off_peak_charging_enabled" "off_peak_charging_weekdays_only" _offPeakEnabled

    s e w = \case
           Never        -> [e := False, w := False]
           Always       -> [e := True, w := False]
           WeekdaysOnly -> [e := True, w := True]

mkNamedCommands [("hvacOn", "auto_conditioning_start"),
                 ("hvacOff", "auto_conditioning_stop"),
                 ("wakeUp", "wake_up")]

