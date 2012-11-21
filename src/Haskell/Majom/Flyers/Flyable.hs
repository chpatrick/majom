-- | Interface for a flyable thing - whether that's some sort of
-- abstraction or a real helicopter or whatnot.

module Majom.Flyers.Flyable (
  -- * Types
  Option(..),
  -- * Classes
  Flyable(..),
  Location(..)
  -- * Functions
  ) where

-- | Options for different commands, relevant to some flying thing.
data Option = Yaw | Pitch | Throttle | Correction
  deriving (Eq, Ord, Show, Enum)

class Location a where
  dimensions :: a -> [Double]

-- | Flyable class for flyable things that can fly.
class Flyable a where
  setFly :: a -> Option -> Int -> IO ()
  setFlyMany :: a -> [(Option, Int)] -> IO ()
  fly :: a -> IO ()
  observe :: a -> IO (Double, Double)
