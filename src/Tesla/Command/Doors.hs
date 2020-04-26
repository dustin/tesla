{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Doors (
  actuateFrontTrunk, actuateRearTrunk,
  lockDoors, unlockDoors
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))
import           Tesla.Command

atr :: MonadIO m => String -> Car m CommandResponse
atr w = runCmd "actuate_trunk" [ "which_trunk" := w ]

actuateFrontTrunk :: MonadIO m => Car m CommandResponse
actuateFrontTrunk = atr "front"

actuateRearTrunk :: MonadIO m => Car m CommandResponse
actuateRearTrunk = atr "rear"

mkNamedCommands [("lockDoors", "door_lock"),
                 ("unlockDoors", "door_unlock")]
