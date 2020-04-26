{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Tesla.Command
Description : Commands executed on a car.

Executing commands within the Car Monad.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tesla.Command (
  runCmd, runCmd', CommandResponse, Car,
  -- * TH support for generating commands.
  mkCommand, mkCommands, mkNamedCommands) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Lens        (key, _Bool, _String)
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text)
import           Language.Haskell.TH
import           Network.Wreq           (Response, asJSON, postWith, responseBody)
import           Network.Wreq.Types     (FormValue (..), Postable)
import           Text.Casing            (fromSnake, toCamel)


import           Tesla
import           Tesla.Car

-- | A CommandResponse wraps an Either such that Left represents a
-- failure message and Right suggests the command was successful.
type CommandResponse = Either Text ()

-- | Run a command with a payload.
runCmd :: (MonadIO m, Postable p) => String -> p -> Car m CommandResponse
runCmd cmd p = do
  a <- authInfo
  v <- currentVehicleID
  r <- liftIO (asJSON =<< postWith (authOpts a) (vehicleURL v $ "command/" <> cmd) p :: IO (Response Value))
  pure $ case r ^? responseBody . key "response" . key "result" . _Bool of
    Just True -> Right ()
    _         -> Left $ r ^. responseBody . key "response" . key "reason" . _String

-- | Run command without a payload
runCmd' :: MonadIO m => String -> Car m CommandResponse
runCmd' cmd = runCmd cmd BL.empty

instance FormValue Bool where
  renderFormValue True  = "true"
  renderFormValue False = "false"


-- | Build a simple named command car that posts to the given named endpoint.
mkCommand :: String -> String -> Q [Dec]
mkCommand s u = do
  let m = mkName "m"
  pure $ [
    SigD (mkName s) (ForallT [PlainTV m] [AppT (ConT (mkName "MonadIO")) (VarT m)]
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

-- Make commands with given names.
mkNamedCommands :: [(String, String)] -> Q [Dec]
mkNamedCommands = cmapM (uncurry mkCommand)
