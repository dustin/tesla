{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tesla.Car.Command.RemoteStart where

import           Control.Monad.IO.Class (MonadIO (..))

import           Tesla.Car.Command

mkCommand "remoteStart" "remote_start_drive"
