-- | Intent to hover in one place, approaching at a max speed
-- and then slowing as the target is being reached.
module Majom.Control.Monkey.HoverIntent (
  hoverAt,
  HoverIntent,
  ) where

import Majom.Common
import Majom.Control.Monkey.Intent

-- | Hover intent data structure, containing the position to hover
-- at.
data HoverIntent = HoverIntent { hoverPosition :: Position }

-- Assuming unit mass...
instance Intent HoverIntent where
  getVel intent pos = 
    ((vectorUnit (getVec dir)) |*| s) 
    where
      dir = q - pos
      dist = vectorSize (getVec dir)
      stop = 0.2
      q = hoverPosition intent
      s = speedMax * (if dist > stop then 1.0 else (dist/stop))
  getHeading i pos = getVec $ (hoverPosition i) - pos
  getIntendedPos = hoverPosition

-- | The maximum speed.
speedMax :: Double
speedMax = 1

-- | Makes a hover intent.
hoverAt :: Position -> HoverIntent
hoverAt = HoverIntent
