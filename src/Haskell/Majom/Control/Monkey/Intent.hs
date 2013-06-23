-- | Module for the intent class - representing an intention
-- to 'do something' and how to do it'.
module Majom.Control.Monkey.Intent (
  Intent(..),
  getYaw,
  ) where

import Majom.Common
import Majom.Flyers.Flyable

-- | The intent class itself. Currently only returns an acceleration
-- in some direction given a velocity and position.
class Intent a where
  enactIntent :: (Flyable b) => a -> b -> Position -> Velocity -> IO a
  success :: a -> Bool

-- | Gets the yaw for a desired orientation given a current position
getYaw :: Vector -> Position -> Int
getYaw v pos =
  if abs (degDiff o o') > 20
    then 
        floor $ 63 + (if abs (degDiff o o') < 40 then (signum (degDiff o o'))*20 else (signum (degDiff o o'))*30)
    else 63
  where
    vec = vectorUnit v
    o = degNorm $ getFacing pos
    o' = 
      if vectorX vec >= 0 && vectorZ vec >= 0 then base
      else if vectorX vec >= 0 then 180 + base
      else if vectorZ vec >= 0 then 360 + base
      else 180 + base
    base = degrees $ atan $ (vectorX vec) / (vectorZ vec)
