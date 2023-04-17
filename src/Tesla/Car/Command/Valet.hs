{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Valet (
  setValetMode, clearValetPIN
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Car.Command

-- | Enable valet mode with a four digit pin.
setValetMode :: MonadIO m => Bool -> Int -> Car m CommandResponse
setValetMode on pin = runCmd "set_valet_mode" [ "on" .= on, "password" .= pin]

mkCommand "clearValetPIN" "reset_valet_pin"
