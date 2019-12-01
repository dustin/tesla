{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Tesla.Command.TH where

import           Language.Haskell.TH
import           Text.Casing

-- Build a simple named command car that posts to the given named endpoint.
carCMD :: String -> String -> Q [Dec]
carCMD s u = do
  let m = mkName "m"
  pure $ [
    SigD (mkName s) (ForallT [PlainTV m] [AppT (ConT (mkName "MonadIO")) (VarT m)]
                     (AppT (AppT (ConT (mkName "Car")) (VarT m)) (ConT (mkName "CommandResponse")))),
    FunD (mkName s) [Clause [] (NormalB expr) []]]
  where expr = LamE [] (AppE (VarE (mkName "runCmd'")) (LitE (StringL u)))

cmapM :: (Monoid b, Applicative f) => (a -> f b) -> [a] -> f b
cmapM f xs = mconcat <$> traverse f xs

-- Build a bunch of commands from a list of named endpoints, defining
-- functions by removing the common prefix.
mkCommands :: [String] -> Q [Dec]
mkCommands targets = cmapM easyCMD targets
  where
    prefix = commonPrefix targets
    easyCMD :: String -> Q [Dec]
    easyCMD target = do
      let s = drop (length prefix) target
          mn = (toCamel . fromSnake) s
      carCMD mn target

    commonPrefix = fmap head . takeWhile (\(x:xs) -> all (== x) xs) . tp
      where
        tp xs
          | any null xs = []
          | otherwise = (head <$> xs) : tp (tail <$> xs)

-- Make commands with given names.
mkNamedCommands :: [(String, String)] -> Q [Dec]
mkNamedCommands = cmapM (uncurry carCMD)
