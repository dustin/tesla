{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Module      : Tesla.Car.Command
Description : Commands executed on a car.

Executing commands within the Car Monad.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tesla.Car.Command (
  Time(..), mkTime, fromTime,
  Percent(..), mkPercent,
  runCmd, runCmd', CommandResponse, Car,
  (.=),
  -- * TH support for generating commands.
  mkCommand, mkCommands, mkNamedCommands) where

import           Control.Lens                    hiding ((.=))
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Lens                 (_Bool, _String, key)
import           Data.Finite                     (Finite, getFinite, modulo, packFinite)
import           Data.Text                       (Text)
import           GHC.Read
import           GHC.TypeNats
import           Language.Haskell.TH
import           Network.Wreq.Types              (FormValue (..))
import           Text.Casing                     (fromSnake, toCamel)

import           Data.Aeson.Types                (Pair)
import           Tesla.Car
import           Tesla.Internal.HTTP
import qualified Text.ParserCombinators.ReadPrec as TextParser

-- | A CommandResponse wraps an Either such that Left represents a
-- failure message and Right suggests the command was successful.
type CommandResponse = Either Text ()

-- | Data type representing local time in minutes since midnight.
newtype Time = Time (Finite 1440)

instance Show Time where show (Time t) = show (toInteger t)

instance Num Time where
  fromInteger = Time . modulo
  abs = id
  signum = const 1
  (Time f1) * (Time f2) = Time (f1 * f2)
  (Time f1) + (Time f2) = Time (f1 + f2)
  (Time f1) - (Time f2) = Time (f1 - f2)

instance FormValue Time where
  renderFormValue (Time x) = renderFormValue (getFinite x)

instance ToJSON Time where
  toJSON (Time x) = toJSON (getFinite x)

-- | Make a 'Time' with the given hours and minutes.
mkTime :: Finite 24 -> Finite 60 -> Time
mkTime h m = Time $ modulo (toInteger h * 60 + toInteger m)

-- | Get the hours and minutes out of a 'Time'.
fromTime :: Time -> (Finite 24, Finite 60)
fromTime (Time t) = bimap f f (t `divMod` 60)
  where
    f :: forall m n. (KnownNat m, KnownNat n, n <= m) => Finite m -> Finite n
    f = modulo . toInteger

-- | A type representing a whole number percnetage between 0 and 100 (inclusive).
newtype Percent = Percent (Finite 101)

instance Read Percent where
    readPrec = TextParser.prec 10 (maybe TextParser.pfail pure . mkPercent @Int =<< readPrec)

instance Show Percent where show (Percent t) = show (toInteger t)

mkPercent :: Integral n => n -> Maybe Percent
mkPercent = fmap Percent . packFinite . toInteger

instance ToJSON Percent where
  toJSON (Percent x) = toJSON (getFinite x)

-- | Run a command with a JSON payload.
runCmd :: MonadIO m => String -> [Pair] -> Car m CommandResponse
runCmd cmd p = do
  v <- currentVehicleID
  j :: Value <- jpostAuth (vehicleURL v $ "command/" <> cmd) (object p)
  pure $ case j ^? key "response" . key "result" . _Bool of
    Just True -> Right ()
    _         -> Left $ j ^. key "response" . key "reason" . _String


-- | Run command without a payload
runCmd' :: MonadIO m => String -> Car m CommandResponse
runCmd' = (`runCmd` [])

instance FormValue Bool where
  renderFormValue True  = "true"
  renderFormValue False = "false"

-- | Build a simple named command car that posts to the given named endpoint.
mkCommand :: String -> String -> Q [Dec]
mkCommand s u = do
  let m = mkName "m"
  pure [
    SigD (mkName s) (ForallT [PlainTV m inferredSpec] [AppT (ConT (mkName "MonadIO")) (VarT m)]
                     (AppT (AppT (ConT (mkName "Car")) (VarT m)) (ConT (mkName "CommandResponse")))),
    FunD (mkName s) [Clause [] (NormalB expr) []]]
  where expr = LamE [] (AppE (VarE (mkName "runCmd'")) (LitE (StringL u)))

cmapM :: (Monoid b, Applicative f) => (a -> f b) -> [a] -> f b
cmapM f xs = mconcat <$> traverse f xs

-- | Build a bunch of commands from a list of named endpoints, defining
-- functions by removing the common prefix.
mkCommands :: [String] -> Q [Dec]
mkCommands targets = cmapM easyCMD targets
  where
    prefix = commonPrefix targets
    easyCMD :: String -> Q [Dec]
    easyCMD target = do
      let s = drop (length prefix) target
          mn = (toCamel . fromSnake) s
      mkCommand mn target

    commonPrefix = fmap head . takeWhile (\(x:xs) -> all (== x) xs) . tp
      where
        tp xs
          | any null xs = []
          | otherwise = (head <$> xs) : tp (tail <$> xs)

-- | Make commands with given names.
mkNamedCommands :: [(String, String)] -> Q [Dec]
mkNamedCommands = cmapM (uncurry mkCommand)
