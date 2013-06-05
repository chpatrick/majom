module Majom.Control.Monkey.LandIntent (
  landOn,
  LandIntent,
  ) where

import Majom.Common
import Majom.Control.Monkey.Intent
import Majom.Control.PID
import Majom.Flyers.Flyable

-- | Hover intent data structure, containing the position to hover
-- at.
data LandIntent = LandIntent { landPosition :: Position }

-- Assuming unit mass...
instance Intent LandIntent where
  enactIntent i flyer pos vel = do
    pid <- getController flyer

    let desiredPos = getVec $ landPosition i
    if (vectorSize $ desiredPos - (getVec pos)) < 0.3
      then do
        setFly flyer Pitch 63
        setFly flyer Throttle 0
        setFly flyer Yaw 63
      else do
        let err = (vectorY desiredPos) - (vectorY $ getVec pos)
        let (pid', m) = getMV pid err

        putStrLn $ show (base + floor m)

        let fwds = vectorSize $ getVel i pos

        setController flyer pid'
        setFly flyer Pitch $ 63 - floor (15 * fwds)
        setFly flyer Throttle $ base + floor m
        setFly flyer Yaw $ getYaw (getHeading i pos) pos

getVel :: LandIntent -> Position -> Velocity
getVel intent pos = 
  ((vectorUnit (getVec dir)) |*| s) 
  where
    dir = q - pos
    dist = vectorSize (getVec dir)
    stop = 0.5
    q = landPosition intent
    s = speedMax * (if dist > stop then 1.0 else (dist/stop))

getHeading :: LandIntent -> Position -> Vector
getHeading i pos = getVec $ (landPosition i) - pos

-- | The maximum speed.
speedMax :: Double
speedMax = 1

base :: Power
base = 80

landOn :: Position -> LandIntent
landOn (Position v o) =
  LandIntent $ Position (vector [vectorX v, vectorY v + 0.2, vectorZ v]) o
