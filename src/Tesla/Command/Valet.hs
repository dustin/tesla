{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Valet (
  setValetMode, clearValetPIN
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))
import           Tesla.Command
import           Tesla.Command.TH

setValetMode :: MonadIO m => Bool -> Int -> Car m CommandResponse
setValetMode on pin = runCmd "set_valet_mode" [ "on" := on, "password" := pin]

carCMD "clearValetPIN" "reset_valet_pin"
