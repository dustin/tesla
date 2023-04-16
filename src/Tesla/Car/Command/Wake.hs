{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Tesla.Car.Command.Wake (
  wakeUp
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BL


import           Tesla.Car              (currentVehicleID, vehicleURL)
import           Tesla.Car.Command
import           Tesla.Internal.HTTP    (jpostAuth)

wakeUp :: MonadIO m => Car m (Maybe Value)
wakeUp = do
  v <- currentVehicleID
  preview (_Just . key "response") <$> jpostAuth @_ @(Maybe Value) (vehicleURL v "wake_up") BL.empty
