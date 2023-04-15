{-|
Module:      Tesla
Description: Tesla API implementation.

'Tesla' is intended to provide access to all known Tesla APIs as
documented at https://www.teslaapi.io/
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tesla
    ( authenticate, refreshAuth, AuthResponse(..),
      Product(..), vehicleName, vehicleID, vehicleState,
      energyID, _ProductVehicle, _ProductEnergy, _ProductPowerwall,
      pwBatteryPower, pwCharged, pwEnergyLeft, pwID, pwName, pwTotal,
      VehicleID, vehicles, products, productsRaw,
      VehicleState(..), vsFromString,
      EnergyID, energyIDs,
      fromToken, authOpts, baseURL,
      decodeProducts
    ) where


import           Control.Lens
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Aeson                 (FromJSON, Value (..), encode)
import           Data.Aeson.Lens            (_Array, _Double, _Integer, _String, key)
import           Data.Foldable              (asum)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.Wreq               (Options, defaults, header)

import           Tesla.Auth
import           Tesla.Internal.HTTP

baseURL :: String
baseURL =  "https://owner-api.teslamotors.com/"
authRefreshURL :: String
authRefreshURL = "https://auth.tesla.com/oauth2/v3/token"
productsURL :: String
productsURL = baseURL <> "api/1/products"

{-# DEPRECATED authenticate "Tesla busted authentication pretty hard.  See https://github.com/dustin/tesla for more info." #-}

-- | Fail to authenticate to the Tesla service.
authenticate :: AuthInfo -> IO AuthResponse
authenticate _ = fail "Tesla busted authentication pretty hard.  See https://github.com/dustin/tesla for more info."

-- | Refresh authentication credentials using a refresh token.
refreshAuth :: AuthResponse -> IO AuthResponse
refreshAuth AuthResponse{..} = do
  jpostWith jOpts authRefreshURL (encode $ Object (mempty
                                                         & at "grant_type" ?~ "refresh_token"
                                                         & at "client_id" ?~ "ownerapi"
                                                         & at "refresh_token" ?~ String (T.pack _refresh_token)
                                                         & at "scope" ?~ "openid email offline_access"
                                                        ))

jOpts :: Options
jOpts = aOpts & header "content-type" .~ ["application/json"]

aOpts :: Options
aOpts = defaults & header "Accept" .~ ["*/*"]

-- | A VehicleID.
type VehicleID = Text

-- | An energy site ID.
type EnergyID = Integer

-- | Possible states a vehicle may be in.
data VehicleState = VOnline | VOffline | VAsleep | VWaking | VUnknown
  deriving (Show, Read, Eq)

vsFromString :: Text -> VehicleState
vsFromString "online"  = VOnline
vsFromString "offline" = VOffline
vsFromString "asleep"  = VAsleep
vsFromString "waking"  = VWaking
vsFromString _         = VUnknown

-- | Tesla Product Types.
data Product = ProductVehicle { _vehicleName :: Text, _vehicleID :: VehicleID, _vehicleState :: VehicleState }
             | ProductEnergy { _energyID :: EnergyID }
             | ProductPowerwall { _pwID           :: EnergyID
                                , _pwBatteryPower :: Double
                                , _pwEnergyLeft   :: Double
                                , _pwCharged      :: Double
                                , _pwName         :: Text
                                , _pwTotal        :: Double }
             deriving (Show, Read, Eq)

makePrisms ''Product
makeLenses ''Product

-- | Decode a products response into a list of products.
decodeProducts :: Value -> [Product]
decodeProducts = catMaybes . toListOf (key "response" . _Array . folded . to prod)
  where
    prod o = asum [ prodCar, prodPowerwall, prodSolar, Nothing ]
      where
        prodCar = ProductVehicle
                  <$> (o ^? key "display_name" . _String)
                  <*> (o ^? key "id_s" . _String)
                  <*> (o ^? key "state" . _String . to vsFromString)
        prodPowerwall = ProductPowerwall
                        <$> (o ^? key "energy_site_id" . _Integer)
                        <*> (o ^? key "battery_power" . _Double)
                        <*> (o ^? key "energy_left" . _Double)
                        <*> (o ^? key "percentage_charged" . _Double)
                        <*> (o ^? key "site_name" . _String)
                        <*> (o ^? key "total_pack_energy" . _Double)
        prodSolar = ProductEnergy <$> (o ^? key "energy_site_id" . _Integer)

-- | productsRaw retrieves the complete response for products
productsRaw :: (FromJSON j, MonadIO m) => AuthInfo -> m j
productsRaw ai = jgetWith (authOpts ai) productsURL

-- | Get all products associated with this account.
products :: MonadIO m => AuthInfo -> m [Product]
products = fmap decodeProducts . productsRaw

-- | Get a mapping of vehicle name to vehicle ID.
vehicles :: [Product] -> Map Text Text
vehicles = Map.fromList . fmap (\(a,b,_) -> (a,b)) . toListOf (folded . _ProductVehicle)

-- | Get a list of Solar ID installations.
energyIDs :: [Product] -> [EnergyID]
energyIDs = toListOf (folded . energyID)
