{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Tesla.Car (vehicleData, VehicleData, VehicleID, vehicleURL,
      isUserPresent, isCharging, teslaTS, maybeTeslaTS,
      Car, runCar, runNamedCar, authInfo, vehicleID,
      Door(..), OpenState(..), doors, openDoors,
      Location(..), DestinationCharger(..), Supercharger(..), Charger(..),
      nearbyChargers, superchargers, destinationChargers,
      lat, lon, _SC, _DC,
      name, location, distance_miles, available_stalls, total_stalls, site_closed
      ) where

import           Control.Lens
import           Control.Monad          ((<=<))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ReaderT (..), asks, runReaderT)
import           Data.Aeson             (FromJSON (..), Options (..),
                                         Result (..), Value (..), decode,
                                         defaultOptions, fieldLabelModifier,
                                         fromJSON, genericParseJSON, withObject,
                                         (.:))
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
import           Network.Wreq           (Response, asJSON, getWith,
                                         responseBody)

import           Tesla

vehicleURL :: VehicleID -> String -> String
vehicleURL v c = mconcat [baseURL, "api/1/vehicles/", v, "/", c]

type VehicleID = String

data CarEnv = CarEnv {
  _authInfo :: IO AuthInfo,
  _vid      :: VehicleID
  }

authInfo :: MonadIO m => Car m AuthInfo
authInfo = liftIO =<< asks _authInfo

vehicleID :: Monad m => Car m VehicleID
vehicleID = asks _vid

type Car = ReaderT CarEnv

runCar :: MonadIO m => IO AuthInfo -> VehicleID -> Car m a -> m a
runCar ai vi f = runReaderT f (CarEnv ai vi)

runNamedCar :: MonadIO m => Text -> IO AuthInfo -> Car m a -> m a
runNamedCar name ai f = do
  a <- liftIO ai
  vs <- vehicles a
  runCar ai (unpack $ vs Map.! name) f

type VehicleData = BL.ByteString

vehicleData :: MonadIO m => Car m VehicleData
vehicleData = do
  a <- authInfo
  v <- vehicleID
  r <- liftIO $ getWith (authOpts a) (vehicleURL v "vehicle_data")
  pure . fromJust . inner $ r ^. responseBody
    where inner = BL.stripPrefix "{\"response\":" <=< BL.stripSuffix "}"

maybeVal :: VehicleData -> Maybe Value
maybeVal = decode

isUserPresent :: VehicleData -> Bool
isUserPresent b = let mb = maybeVal b ^? _Just . key "vehicle_state" . key "is_user_present" . _Bool in
                    fromMaybe False mb

isCharging :: VehicleData -> Bool
isCharging b = let mi = maybeVal b ^? _Just . key "charge_state" . key "charger_power" . _Integer in
                 fromMaybe 0 mi > 0

maybeTeslaTS :: VehicleData -> Maybe UTCTime
maybeTeslaTS b = pt <$> mv
  where mv = maybeVal b ^? _Just . key "vehicle_state" . key "timestamp" . _Integer
        pt x = posixSecondsToUTCTime . fromRational $ x % 1000

teslaTS :: VehicleData -> UTCTime
teslaTS b = fromMaybe (error . show $ b) . maybeTeslaTS $ b

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

isOpen :: OpenState a -> Bool
isOpen (Closed _) = False
isOpen _          = True

fromOpenState :: OpenState a -> a
fromOpenState (Open d)   = d
fromOpenState (Closed d) = d

doors :: VehicleData -> Maybe [OpenState Door]
doors b = traverse ds $ zip ["df", "dr", "pf", "pr", "ft", "rt"] [minBound..]
  where
    ds (k,d) = c d <$> maybeVal b ^? _Just . key "vehicle_state" . key k . _Integer
    c d 0 = Closed d
    c d _ = Open   d

openDoors :: VehicleData -> [Door]
openDoors b = maybe [] (map fromOpenState . filter isOpen) (doors b)

data Location = Location { _lat :: Double, _lon :: Double } deriving (Show, Generic)

makeLenses ''Location

instance FromJSON Location where
  parseJSON = withObject "location" $ \v -> Location <$> v .: "lat" <*> v .: "long"

chargeOpts :: Data.Aeson.Options
chargeOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

data DestinationCharger = DestinationCharger {
  _location       :: Location,
  _name           :: Text,
  _distance_miles :: Double
  } deriving (Show, Generic)

makeFieldsNoPrefix ''DestinationCharger

instance FromJSON DestinationCharger where
  parseJSON = genericParseJSON chargeOpts

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

data Charger = SC Supercharger | DC DestinationCharger deriving(Show)

makePrisms ''Charger

superchargers :: [Charger] -> [Supercharger]
superchargers = toListOf (folded . _SC)

destinationChargers :: [Charger] -> [DestinationCharger]
destinationChargers = toListOf (folded . _DC)

nearbyChargers :: MonadIO m => Car m [Charger]
nearbyChargers = do
  a <- authInfo
  v <- vehicleID
  r <- liftIO (asJSON =<< getWith (authOpts a) (vehicleURL v "nearby_charging_sites") :: IO (Response Value))
  let rb = r ^. responseBody
      chargers = parseOne rb SC "superchargers" <> parseOne rb DC "destination_charging"
  pure (V.toList chargers)

    where
      parseOne rb f k =  let rs = traverse fromJSON (rb ^. key "response" . key k . _Array) in
                           f <$> case rs of
                                   Error e   -> error e
                                   Success s -> s
