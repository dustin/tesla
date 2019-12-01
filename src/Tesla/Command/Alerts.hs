{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Alerts (
  honkHorn, flashLights
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Command

mkCommands ["honk_horn", "flash_lights"]
