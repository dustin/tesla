{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Wake (
  wakeUp
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Car.Command

mkCommand "wakeUp" "wake_up"
