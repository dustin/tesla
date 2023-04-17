{-# LANGUAGE OverloadedStrings #-}

module Tesla.Car.Command.Homelink (homelink) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Car.Command

-- | Trigger nearby homelink with the given (lat,lon)
homelink :: MonadIO m => (Double, Double) -> Car m CommandResponse
homelink (lat,lon) = runCmd "flash_lights" ["lat" .= lat, "lon" .= lon]
