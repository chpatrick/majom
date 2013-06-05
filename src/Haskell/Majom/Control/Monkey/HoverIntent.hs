-- | Intent to hover in one place, approaching at a max speed
-- and then slowing as the target is being reached.
module Majom.Control.Monkey.HoverIntent (
  hoverAt,
  HoverIntent,
  ) where

import Majom.Common
import Majom.Control.Monkey.Intent
import Majom.Control.PID
import Majom.Flyers.Flyable

-- | Hover intent data structure, containing the position to hover
-- at.
data HoverIntent = HoverIntent { hoverPosition :: Position }

-- Assuming unit mass...
instance Intent HoverIntent where
  enactIntent i flyer pos vel = do
    pid <- getController flyer

    let desiredPos = getVec $ hoverPosition i
    let err = (vectorY desiredPos) - (vectorY $ getVec pos)
    let (pid', m) = getMV pid err

    putStrLn $ show (base + floor m)

    let fwds = vectorSize $ getVel i pos

    setController flyer pid'
    setFly flyer Pitch $ 63 - floor (15 * fwds)
    setFly flyer Throttle $ base + floor m
    setFly flyer Yaw $ getYaw (getHeading i pos) pos

getVel :: HoverIntent -> Position -> Velocity
getVel intent pos = 
  ((vectorUnit (getVec dir)) |*| s) 
  where
    dir = q - pos
    dist = vectorSize (getVec dir)
    stop = 0.2
    q = hoverPosition intent
    s = speedMax * (if dist > stop then 1.0 else (dist/stop))

getHeading :: HoverIntent -> Position -> Vector
getHeading i pos = getVec $ (hoverPosition i) - pos

-- | The maximum speed.
speedMax :: Double
speedMax = 1

base :: Power
base = 80

-- | Makes a hover intent.
hoverAt :: Position -> HoverIntent
hoverAt = HoverIntent
