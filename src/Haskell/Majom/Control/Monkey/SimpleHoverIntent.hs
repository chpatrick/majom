-- | Intent to hover in one place, approaching at a max speed
-- and then slowing as the target is being reached.
module Majom.Control.Monkey.SimpleHoverIntent (
  simpleHoverAt,
  SimpleHoverIntent,
  ) where

import Majom.Common
import Majom.Control.Monkey.Intent

-- | Hover intent data structure, containing the position to hover
-- at.
data SimpleHoverIntent = SimpleHoverIntent { hoverPosition :: Position }

-- Assuming unit mass...
instance Intent SimpleHoverIntent where
  getAccel intent v pos
    | dist < 1  = vector [0,0,0]
    | otherwise = (vectorUnit $ getVec dir) |/| 2
    where
      dist = vectorSize $ getVec dir
      dir = q - pos
      q = hoverPosition intent
  getHeading i pos = getVec $ (hoverPosition i) - pos

-- | Makes a hover intent.
simpleHoverAt :: Position -> SimpleHoverIntent
simpleHoverAt = SimpleHoverIntent
