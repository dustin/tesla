{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Charging (
  startCharging, stopCharging, setLimit, openChargePort, closeChargePort
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Command
import           Tesla.Command.TH

setLimit :: MonadIO m => Int -> Car m CommandResponse
setLimit to = runCmd "set_charge_limit" ["percent" := to ]

mkNamedCommands [("startCharging", "charge_start"),
                 ("stopCharging", "charge_stop"),
                 ("openChargePort", "charge_port_door_open"),
                 ("closeChargePort", "charge_port_door_close")]
