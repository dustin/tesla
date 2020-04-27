{-|
Module:      Tesla.Car
Description: Tesla car-specific APIs.

Access of car-specific APIs.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Tesla.Car (
  -- * Car Monad and related types.
  Car, runCar, runNamedCar,
  VehicleID,
  -- * Requests
  vehicleData, nearbyChargers,
  -- * Convenience functions for examining VehicleData
  VehicleData, isUserPresent, isCharging, teslaTS, maybeTeslaTS,
  Door(..), OpenState(..), _Open, _Closed, doors, openDoors,
  -- * Charger Info
  Location(..), DestinationCharger(..), Supercharger(..), Charger(..),
  superchargers, destinationChargers,
  -- * Lenses
  lat, lon, _SC, _DC,
  name, location, distance_miles, available_stalls, total_stalls, site_closed,
  -- * Probably uninteresting internals
  vehicleURL, currentVehicleID
      ) where

import           Control.Exception      (Exception, throwIO)
import           Control.Lens
import           Control.Monad          ((<=<))
import           Control.Monad.Catch    (MonadCatch (..), MonadMask (..), MonadThrow (..))
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ReaderT (..), asks, runReaderT)
import           Data.Aeson             (FromJSON (..), Options (..), Result (..), Value (..), decode, defaultOptions,
                                         fieldLabelModifier, fromJSON, genericParseJSON, withObject, (.:))
import           Data.Aeson.Lens        (key, _Array, _Bool, _Integer)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Ratio
import           Data.Text              (Text, unpack)
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import qualified Data.Vector            as V
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (getWith, responseBody)

import           Tesla
import           Tesla.Auth
import           Tesla.Internal.HTTP

-- | Get the URL for a named endpoint for a given vehicle.
vehicleURL :: VehicleID -> String -> String
vehicleURL v c = mconcat [baseURL, "api/1/vehicles/", unpack v, "/", c]

data CarEnv = CarEnv {
  _authInfo :: IO AuthInfo,
  _vid      :: VehicleID
  }

-- | Get the current vehicle ID from the Car Monad.
currentVehicleID :: MonadReader CarEnv m => m VehicleID
currentVehicleID = asks _vid

-- | Car Monad for accessing car-specific things.
newtype Car m a = Car { runCarM :: ReaderT CarEnv m a }
  deriving (Applicative, Functor, Monad, MonadIO,
            MonadCatch, MonadThrow, MonadMask, MonadReader CarEnv, MonadFail)

instance (Monad m, MonadIO m, MonadReader CarEnv m) => HasTeslaAuth m where
  teslaAuth = liftIO =<< asks _authInfo

-- | Run a Car Monad with the given Vehicle ID
runCar :: MonadIO m => IO AuthInfo -> VehicleID -> Car m a -> m a
runCar ai vi f = runReaderT (runCarM f) (CarEnv ai vi)

newtype BadCarException = BadCar String deriving Eq

instance Show BadCarException where
  show (BadCar s) = "BadCar: " <> s

instance Exception BadCarException

-- | Run a Car Monad by looking up a car by name.
runNamedCar :: MonadIO m => Text -> IO AuthInfo -> Car m a -> m a
runNamedCar name ai f = do
  a <- liftIO ai
  vs <- vehicles a
  c <- case Map.lookup name vs of
         Nothing -> throw $ mconcat [show name, " is not a valid vehicle name.  Try one of: ",
                                     show $ Map.keys vs]
         Just c -> pure c
  runCar ai c f

  where
    throw = liftIO . throwIO . BadCar

-- | Giant blob of VehicleData describing all known state of the vehicle.
--
-- This is not separated into discrete
-- A VehicleIDfields because that's easy enough to do with Aeson and
-- Lens when you need it but some convenience methods for common
-- | A VehicleIDaccesses are available in this module.
type VehicleData = BL.ByteString

-- | Fetch the VehicleData.
vehicleData :: MonadIO m => Car m VehicleData
vehicleData = do
  a <- teslaAuth
  v <- currentVehicleID
  r <- liftIO $ getWith (authOpts a) (vehicleURL v "vehicle_data")
  pure . fromJust . inner $ r ^. responseBody
    where inner = BL.stripPrefix "{\"response\":" <=< BL.stripSuffix "}"

-- | Get an Aeson Value from this VehicleData.
maybeVal :: VehicleData -> Maybe Value
maybeVal = decode

-- | True if a user is present in the vehicle.
isUserPresent :: VehicleData -> Bool
isUserPresent = fromMaybe False . preview (_Just . key "vehicle_state" . key "is_user_present" . _Bool) . maybeVal

-- | True of the vehicle is currently charging.
isCharging :: VehicleData -> Bool
isCharging = maybe False (> 0) . preview (_Just . key "charge_state" . key "charger_power" . _Integer) . maybeVal

-- | Get the timestamp from this VehicleData if present.
maybeTeslaTS :: VehicleData -> Maybe UTCTime
maybeTeslaTS b = maybeVal b ^? _Just . key "vehicle_state" . key "timestamp" . _Integer . to pt
  where pt x = posixSecondsToUTCTime . fromRational $ x % 1000

-- | Get the timestamp from this VehicleData or error if there isn't one.
teslaTS :: VehicleData -> UTCTime
teslaTS b = fromMaybe (error . show $ b) . maybeTeslaTS $ b

-- | The various doors.
data Door = DriverFront
          | DriverRear
          | PassengerFront
          | PassengerRear
          | FrontTrunk
          | RearTrunk
          deriving (Show, Bounded, Enum, Eq)

-- I only care about 0, but these are the observed values:
-- 0 or 1 for df
-- 0 or 2 for pf
-- 0 or 4 for dr
-- 0 or 8 for pr
-- 0 or 16 for ft
-- 0 or 32 for rt
data OpenState a = Closed a | Open a deriving (Show, Eq)

makePrisms ''OpenState

-- | Return a list of doors and their OpenState.
doors :: VehicleData -> Maybe [OpenState Door]
doors b = traverse ds $ zip ["df", "dr", "pf", "pr", "ft", "rt"] [minBound..]
  where
    ds (k,d) = c d <$> maybeVal b ^? _Just . key "vehicle_state" . key k . _Integer
    c d 0 = Closed d
    c d _ = Open   d

-- | Return a list of open doors.
openDoors :: VehicleData -> [Door]
openDoors = toListOf (_Just . folded . _Open) . doors

-- | Location, Location, Location.
data Location = Location { _lat :: Double, _lon :: Double } deriving (Show, Generic)

makeLenses ''Location

instance FromJSON Location where
  parseJSON = withObject "location" $ \v -> Location <$> v .: "lat" <*> v .: "long"

chargeOpts :: Data.Aeson.Options
chargeOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

-- | A destination charger (provided by nearbyChargers).
data DestinationCharger = DestinationCharger {
  _location       :: Location,
  _name           :: Text,
  _distance_miles :: Double
  } deriving (Show, Generic)

makeFieldsNoPrefix ''DestinationCharger

instance FromJSON DestinationCharger where
  parseJSON = genericParseJSON chargeOpts

-- | A supercharger (provided by nearbyChargers).
data Supercharger = Supercharger {
  _location         :: Location,
  _name             :: Text,
  _distance_miles   :: Double,
  _available_stalls :: Int,
  _total_stalls     :: Int,
  _site_closed      :: Bool
  } deriving(Show, Generic)

makeFieldsNoPrefix ''Supercharger

instance FromJSON Supercharger where
  parseJSON = genericParseJSON chargeOpts

-- | Eitehr a Supercharger or Destination charger.
data Charger = SC Supercharger | DC DestinationCharger deriving(Show)

makePrisms ''Charger

-- | Return only the superchargers from a Charger list.
superchargers :: [Charger] -> [Supercharger]
superchargers = toListOf (folded . _SC)

-- | Return only the destination chargers from a Charger list.
destinationChargers :: [Charger] -> [DestinationCharger]
destinationChargers = toListOf (folded . _DC)

-- | Get the nearby chargers.
nearbyChargers :: MonadIO m => Car m [Charger]
nearbyChargers = do
  v <- currentVehicleID
  rb <- jgetAuth (vehicleURL v "nearby_charging_sites")
  let chargers = parseOne rb SC "superchargers" <> parseOne rb DC "destination_charging"
  pure (V.toList chargers)

    where
      parseOne :: FromJSON a => Value -> (a -> Charger) -> Text -> V.Vector Charger
      parseOne rb f k =  let rs = traverse fromJSON (rb ^. key "response" . key k . _Array) in
                           f <$> case rs of
                                   Error e   -> error e
                                   Success s -> s
