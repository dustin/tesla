{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Doors (
  lock, unlock
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Tesla.Command

lock :: MonadIO m => Car m CommandResponse
lock = runCmd' "door_lock"

unlock :: MonadIO m => Car m CommandResponse
unlock = runCmd' "door_unlock"
