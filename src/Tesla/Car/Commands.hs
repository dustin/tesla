{-|
Module:      Tesla.Commands
Description: Convenience module for importing all commands at once.

All of the commands supported by this library.

This is meant to be imported qualified, e.g.:

> import qualified Tesla.Commands as CMD
-}

module Tesla.Car.Commands ( module CMD ) where

import           Tesla.Car.Command.Alerts     as CMD
import           Tesla.Car.Command.Charging   as CMD
import           Tesla.Car.Command.Climate    as CMD
import           Tesla.Car.Command.Doors      as CMD
import           Tesla.Car.Command.Homelink   as CMD
import           Tesla.Car.Command.Media      as CMD
import           Tesla.Car.Command.Sentry     as CMD
import           Tesla.Car.Command.Sharing    as CMD
import           Tesla.Car.Command.Software   as CMD
import           Tesla.Car.Command.SpeedLimit as CMD
import           Tesla.Car.Command.Valet      as CMD
import           Tesla.Car.Command.Wake       as CMD
import           Tesla.Car.Command.Windows    as CMD
