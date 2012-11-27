-- | Common functions and types for majom.
module Majom.Common where

instance (Num a, Num b) => Num (a,b) where
  (x1,x2) + (y1,y2) = (x1+y1,x2+y2)
  (x1,x2) * (y1,y2) = (x1*y1,x2*y2)
  negate (x,y) = (negate x, negate y)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

-- | Vector scaling function.
(|*|) :: Vector -> Double -> Vector
(|*|) v d = v * vector2 d d

-- | Vector constructor
vector2 :: Double -> Double -> Vector
vector2 x y = (x, y)

-- | Simple Vector, made from doubles.
type Vector = (Double, Double)

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
gravity = vector2 0 (-9.8)
