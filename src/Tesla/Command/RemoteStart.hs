{-# LANGUAGE OverloadedStrings #-}

module Tesla.Command.RemoteStart where

import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wreq           (FormParam (..))

import           Tesla.Command

-- | Enables keyless driving.
--
-- This requires your account password, so, you know, be careful where you use it.
remoteStart :: MonadIO m => String -> Car m CommandResponse
remoteStart pw = runCmd "remote_start_drive" ["password" := pw]
