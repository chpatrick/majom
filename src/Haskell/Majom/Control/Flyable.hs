module Majom.Control.Flyable (
  -- * Types
  Option(..),
  -- * Classes
  Flyable(..)
  -- * Functions
  ) where

data Option = Yaw | Pitch | Throttle | Correction
  deriving (Eq, Ord, Show, Enum)

class Flyable a where
  setFly :: a -> Option -> Int -> IO ()
  setFlyMany :: a -> [(Option, Int)] -> IO ()
