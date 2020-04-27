{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Valet (
  setValetMode, clearValetPIN
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Car.Command

setValetMode :: MonadIO m => Bool -> Int -> Car m CommandResponse
setValetMode on pin = runCmd "set_valet_mode" [ "on" := on, "password" := pin]

mkCommand "clearValetPIN" "reset_valet_pin"
