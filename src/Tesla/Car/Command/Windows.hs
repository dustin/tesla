{-# LANGUAGE OverloadedStrings #-}

module Tesla.Car.Command.Windows (
  ventWindows, closeWindows, ventSunroof, closeSunroof
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)
import           Network.Wreq           (FormParam (..))

import           Tesla.Car.Command

windowControl :: MonadIO m => Text -> (Double, Double) -> Car m CommandResponse
windowControl x (lat,lon) = runCmd "window_control" [ "command" := x, "lat" := lat, "lon" := lon]

ventWindows :: MonadIO m => Car m CommandResponse
ventWindows = windowControl "vent" (0,0)

closeWindows :: MonadIO m => (Double, Double) -> Car m CommandResponse
closeWindows = windowControl "close"

sc :: MonadIO m => Text -> Car m CommandResponse
sc c = runCmd "sun_roof_control" [ "state" := c ]

ventSunroof :: MonadIO m => Car m CommandResponse
ventSunroof = sc "vent"

closeSunroof :: MonadIO m => Car m CommandResponse
closeSunroof = sc "close"
