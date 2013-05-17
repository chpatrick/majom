-- | Module that implements a Kalman filter model.
module Majom.Analysis.AdLib(
  -- * Types
  AdLib(..),
  -- * Functions
  adlib,
  ) where

import Majom.Analysis.Model
import Majom.Common

data AdLib = AdLib { adlibPwr :: Power }

instance Model AdLib where
  createNewModel =
    AdLib 0 
  getMap k x
    | vectorY x < 0 = (adlibPwr k) - 10
    | vectorY x > 0 = (adlibPwr k) + 10
    | otherwise     = adlibPwr k
  updateModel k (pwr,vel)
    | v > 0 && pwr <= p = k--AdLib (pwr - 1)
    | v < 0 && pwr >= p = k--AdLib (pwr + 1)
    | otherwise         = k
    where 
      v = vectorY vel
      p = adlibPwr k

adlib :: AdLib -> Power -> Velocity -> Velocity -> AdLib
adlib a pwr desiredV obtainedV
  | dv > ov = AdLib (p + 1)
  | dv < ov = AdLib (p - 1)
  | otherwise = a
  where
    dv = getDirection desiredV --vectorY desiredV
    ov = getDirection obtainedV --vectorY obtainedV
    p  = adlibPwr a
