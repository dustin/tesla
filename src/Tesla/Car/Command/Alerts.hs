{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Alerts (
  honkHorn, flashLights
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Car.Command

mkCommands ["honk_horn", "flash_lights"]
