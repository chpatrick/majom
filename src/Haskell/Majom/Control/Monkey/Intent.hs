-- | Module for the intent class - representing an intention
-- to 'do something' and how to do it'.
module Majom.Control.Monkey.Intent (
  Intent(..),
  ) where

import Majom.Common

-- | The intent class itself. Currently only returns an acceleration
-- in some direction given a velocity and position.
class Intent a where
  getVel :: a -> Position -> Velocity
  getHeading :: a -> Position -> Vector
  getIntendedPos :: a -> Position
