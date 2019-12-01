{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tesla.Command (runCmd, runCmd', CommandResponse, Car) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Lens        (key, _Bool, _String)
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text)
import           Network.Wreq           (Response, asJSON, postWith,
                                         responseBody)
import           Network.Wreq.Types     (FormValue (..), Postable)

import           Tesla
import           Tesla.Car

type CommandResponse = Either Text ()

-- | Run a command with a payload.
runCmd :: (MonadIO m, Postable p) => String -> p -> Car m CommandResponse
runCmd cmd p = do
  a <- authInfo
  v <- vehicleID
  r <- liftIO (asJSON =<< postWith (authOpts a) (vehicleURL v $ "command/" <> cmd) p :: IO (Response Value))
  pure $ case r ^? responseBody . key "response" . key "result" . _Bool of
    Just True  -> Right ()
    _ -> Left $ r ^. responseBody . key "response" . key "reason" . _String

-- | run command without a payload
runCmd' :: MonadIO m => String -> Car m CommandResponse
runCmd' cmd = runCmd cmd BL.empty

instance FormValue Bool where
  renderFormValue True  = "true"
  renderFormValue False = "false"
