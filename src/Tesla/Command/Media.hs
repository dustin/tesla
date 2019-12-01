{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Command.Media where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Command
import           Tesla.Command.TH

mkCommands [
  "media_toggle_playback",
  "media_next_track",
  "media_prev_track",
  "media_next_fav",
  "media_prev_fav",
  "media_volume_up",
  "media_volume_down"
  ]
