{-|
Module:      Tesla
Description: Tesla API implementation.

'Tesla' is intended to provide access to all known Tesla APIs as
documented at https://tesla-api.timdorr.com/
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tesla
    ( authenticate, refreshAuth, AuthResponse(..),
      Product(..), vehicleName, vehicleID, energyID, _ProductVehicle, _ProductEnergy, _ProductPowerWall,
      VehicleID, vehicles, products,
      EnergyID, energyIDs,
      fromToken, authOpts, baseURL,
      decodeProducts
    ) where


import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (Value (..))
import           Data.Aeson.Lens        (key, _Array, _Integer, _String)
import           Data.Foldable          (asum)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Network.Wreq           (FormParam (..))

import           Tesla.Auth
import           Tesla.Internal.HTTP

baseURL :: String
baseURL = "https://owner-api.teslamotors.com/"
authURL :: String
authURL = baseURL <> "oauth/token"
authRefreshURL :: String
authRefreshURL = baseURL <> "oauth/token"
productsURL :: String
productsURL = baseURL <> "api/1/products"


-- | Authenticate to the Tesla service.
authenticate :: AuthInfo -> IO AuthResponse
authenticate AuthInfo{..} =
  jpostWith defOpts authURL ["grant_type" := ("password" :: String),
                             "client_id" := _clientID,
                             "client_secret" := _clientSecret,
                             "email" := _email,
                             "password" := _password]

-- | Refresh authentication credentials using a refresh token.
refreshAuth :: AuthInfo -> AuthResponse -> IO AuthResponse
refreshAuth AuthInfo{..} AuthResponse{..} =
  jpostWith defOpts authRefreshURL ["grant_type" := ("refresh_token" :: String),
                                    "client_id" := _clientID,
                                    "client_secret" := _clientSecret,
                                    "refresh_token" := _refresh_token]


-- | A VehicleID.
type VehicleID = Text

-- | An energy site ID.
type EnergyID = Integer

-- | Tesla Product Types.
data Product = ProductVehicle { _vehicleName :: Text, _vehicleID :: VehicleID }
             | ProductEnergy { _energyID :: EnergyID }
             | ProductPowerWall deriving (Show, Read, Eq)

makePrisms ''Product
makeLenses ''Product

decodeProducts :: Value -> [Product]
decodeProducts = catMaybes . toListOf (key "response" . _Array . folded . to prod)
  where
    prod o = asum [ prodCar, prodSolar, Nothing ]
      where
        prodCar = ProductVehicle <$> (o ^? key "display_name" . _String) <*> (o ^? key "id_s" . _String)
        prodSolar = ProductEnergy <$> (o ^? key "energy_site_id" . _Integer)

-- | Get all products associated with this account.
products :: MonadIO m => AuthInfo -> m [Product]
products ai = decodeProducts <$> jgetWith (authOpts ai) productsURL

-- | Get a mapping of vehicle name to vehicle ID.
vehicles :: [Product] -> (Map Text Text)
vehicles = Map.fromList . toListOf (folded . _ProductVehicle)

-- | Get a list of Solar ID installations.
energyIDs :: [Product] -> [EnergyID]
energyIDs = toListOf (folded . energyID)
