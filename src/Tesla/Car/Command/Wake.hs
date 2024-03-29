{-# LANGUAGE OverloadedStrings #-}

module Tesla.Car.Command.Wake (
  wakeUp
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BL


import           Tesla.Car              (currentVehicleID, vehicleURL)
import           Tesla.Car.Command
import           Tesla.Internal.HTTP    (jpostAuth)

-- | Request the car to wake up.
-- This function should return pretty quickly, but the car may take a few seconds to wake up.
wakeUp :: (FromJSON j, MonadIO m) => Car m j
wakeUp = currentVehicleID >>= \v -> jpostAuth (vehicleURL v "wake_up") BL.empty
