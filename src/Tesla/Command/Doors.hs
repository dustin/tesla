{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Doors (
  actuateFrontTrunk, actuateRearTrunk
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
