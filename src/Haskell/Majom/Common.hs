-- | Common functions and types for majom.
module Majom.Common where

import qualified Data.Vector as V

instance (Num a, Num b) => Num (a,b) where
  (x1,x2) + (y1,y2) = (x1+y1,x2+y2)
  (x1,x2) * (y1,y2) = (x1*y1,x2*y2)
  negate (x,y) = (negate x, negate y)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

instance (Num a) => Num (V.Vector a) where
  as + bs = V.zipWith (+) as bs
  as - bs = V.zipWith (-) as bs
  as * bs = V.zipWith (*) as bs
  negate a = V.map negate a
  fromInteger x = error "Cannot instantiate Vector from Integer :("
  abs m = V.map abs m
  signum _ = 1

-- | Vector scaling function.
(|*|) :: Vector -> Double -> Vector
(|*|) v d = V.map (*d) v

-- | Inverse Vector scaling function.
(|/|) :: Vector -> Double -> Vector
(|/|) v d = V.map (/d) v

-- | Unit a vector
vectorUnit :: Vector -> Vector
vectorUnit v = v |/| (vectorSize v)

-- | Size a vector
vectorSize :: Vector -> Double
vectorSize v = sqrt $ V.sum $ v * v

-- | Vector constructor
vector2 :: Double -> Double -> Vector
vector2 x y = V.fromList [x, y]

-- | Creates a vector from a list.
vector :: [Double] -> Vector
vector = V.fromList

-- | Gets the X value of a vector.
vectorX :: Vector -> Double
vectorX = (V.! 0)

-- | Gets the Y value of a vector.
vectorY :: Vector -> Double
vectorY = (V.! 1)

-- | Gets the Z value of a vector.
vectorZ :: Vector -> Double
vectorZ = (V.! 2)

prettyVec :: Vector -> String
prettyVec v = 
  if V.length v /= 3
    then error "Vector is too short to pretty print..."
    else show (v V.! 0, v V.! 1, v V.! 2)

-- | Simple Vector, made from doubles.
type Vector = V.Vector Double

-- | Position for objects.
type Position = Vector

-- | Velocity for objects.
type Velocity = Vector

-- | Acceleration for objects.
type Acceleration = Vector

-- | Force for objects.
type Force = Vector

-- | Time for simulation (in seconds)
type Time = Double

-- | Power from the controller
type Power = Int

-- | Gravity force constant
gravity :: Force
gravity = vector [0, (-9.8), 0]

-- | Calcules the acceleration at a given observation time, given 
-- a string of observed events.
-- There is an implicit ordering, with newest observations at the front.
calcObservedAccel :: [(Power, Position, Time)] -> [(Power, Acceleration)]
calcObservedAccel obs = if length obs < 3 then [] else
  calc x
  where
    x = reverse obs

    calc :: [(Power, Position, Time)] -> [(Power, Acceleration)]
    calc v@((_, pos'', t''):(_, pos', t'):(pwr, pos, t):vs) =
      (pwr, accel) : calc (tail v)
      where
        vel' = (pos' - pos'') |/| (t'' - t')
        vel  = (pos - pos') |/| (t' - t)
        accel = (vel - vel') |/| (t' - t)
    calc vs = []

-- | Tolerance for use with approximations.
tolerance :: Double
tolerance = 1e-5

-- | Approximately equals.
(~=) :: Double -> Double -> Bool
(~=) a b = abs (a - b) < tolerance
