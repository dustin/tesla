{-|
Module:      Tesla.Energy
Description: Tesla energy-specific APIs.

Access of energy-specific APIs.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Tesla.Energy (
  runEnergy, siteData, siteConfig, Energy
  ) where

import           Control.Exception       (Exception)
import           Control.Monad.Catch     (MonadCatch (..), MonadMask (..), MonadThrow (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Control.Monad.Logger    (MonadLogger)
import           Control.Monad.Reader    (MonadReader, ReaderT (..), asks, runReaderT)
import           Data.Aeson              (FromJSON (..))

import           Tesla
import           Tesla.Auth
import           Tesla.Internal.HTTP

-- | Get the URL for a named endpoint for a given vehicle.
energyURL :: EnergyID -> String -> String
energyURL v c = mconcat [baseURL, "api/1/energy_sites/", show v, "/", c]

data EnergyEnv = EnergyEnv {
  _authInfo :: IO AuthInfo,
  _eid      :: EnergyID
  }

-- | Get the current energy ID from the Energy Monad.
currentEnergyID :: Monad m => Energy m EnergyID
currentEnergyID = asks _eid

-- | Energy Monad for accessing energy-specific things.
newtype Energy m a = Energy { runEnergyM :: ReaderT EnergyEnv m a }
  deriving (Applicative, Functor, Monad, MonadIO,
            MonadCatch, MonadThrow, MonadMask, MonadReader EnergyEnv,
            MonadFail, MonadLogger)

instance MonadUnliftIO m => MonadUnliftIO (Energy m) where
  withRunInIO inner = Energy $ withRunInIO $ \run -> inner (run . runEnergyM)

instance (Monad m, MonadIO m, MonadReader EnergyEnv m) => HasTeslaAuth m where
  teslaAuth = liftIO =<< asks _authInfo

-- | Run a Energy Monad with the given Vehicle ID
runEnergy :: MonadIO m => IO AuthInfo -> EnergyID -> Energy m a -> m a
runEnergy ai ei f = runReaderT (runEnergyM f) (EnergyEnv ai ei)

newtype BadEnergyException = BadEnergy String deriving Eq

instance Show BadEnergyException where
  show (BadEnergy s) = "BadEnergy: " <> s

instance Exception BadEnergyException

{- 404?
siteSummary :: (FromJSON j, MonadIO m) => Energy m j
siteSummary = currentEnergyID >>= \e -> jget (energyURL e "status")
-}

-- | Fetch the "live_status" describing the current active state of an
-- energy site.
siteData :: (FromJSON j, MonadIO m) => Energy m j
siteData = currentEnergyID >>= \e -> jgetAuth (energyURL e "live_status")

-- | Fetch the "site_info" describing the basic configuration of an energy site.
siteConfig :: (FromJSON j, MonadIO m) => Energy m j
siteConfig = currentEnergyID >>= \e -> jgetAuth (energyURL e "site_info")

{- Errors that 'kind' is required.
siteHistory :: (FromJSON j, MonadIO m) => Energy m j
siteHistory = currentEnergyID >>= \e -> jget (energyURL e "history")
-}
