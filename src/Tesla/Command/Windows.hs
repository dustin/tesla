{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.Windows (
  ventWindows, closeWindows
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)
import           Network.Wreq           (FormParam (..))
import           Tesla.Command

windowControl :: MonadIO m => Text -> (Double, Double) -> Car m CommandResponse
windowControl x (lat,lon) = runCmd "window_control" [ "command" := x, "lat" := lat, "lon" := lon]

ventWindows :: MonadIO m => Car m CommandResponse
ventWindows = windowControl "vent" (0,0)

closeWindows :: MonadIO m => (Double, Double) -> Car m CommandResponse
closeWindows = windowControl "close"
