-- | Interface for a flyable thing - whether that's some sort of
-- abstraction or a real helicopter or whatnot.

module Majom.Flyers.Flyable (
  -- * Types
  Option(..),
  -- * Classes
  Flyable(..),
  -- * Functions
  flyerInit,
  ) where

import Majom.Common
import Majom.Control.PID

-- | Options for different commands, relevant to some flying thing.
data Option = Yaw | Pitch | Throttle | Correction
  deriving (Eq, Ord, Show, Enum)

-- | Initiates the default flyer options
flyerInit :: [(Option, Int)]
flyerInit = [(Yaw, 63), (Pitch, 63), (Throttle, 0), (Correction, 63)]

-- | Flyable class for flyable things that can fly.
class Flyable a where
  setFly :: a -> Option -> Int -> IO ()
  setFlyMany :: a -> [(Option, Int)] -> IO ()
  fly :: a -> IO ()
  observe :: a -> IO (Power, Position)
  isActive :: a -> IO Bool
  setActive :: a -> Bool -> IO ()
  getController :: a -> IO PID
  setController :: a -> PID -> IO ()
  lookAround :: a -> IO [Surface] 
