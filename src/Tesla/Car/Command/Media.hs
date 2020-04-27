{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.Media where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Car.Command

mkCommands [
  "media_toggle_playback",
  "media_next_track",
  "media_prev_track",
  "media_next_fav",
  "media_prev_fav",
  "media_volume_up",
  "media_volume_down"
  ]
