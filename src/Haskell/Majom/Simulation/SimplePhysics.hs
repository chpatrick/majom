-- | A module for performing simple physics simulations. 
-- Will be useful for testing helicopter controls using an
-- abstracted environment.
module Majom.Simulation.SimplePhysics (
  -- * Types
  Object(..),
  Force,
  Time,
  Vector,
  Velocity,
  Acceleration,
  Position,
  -- * Functions
  vector,
  updatePosition
  ) where

-- | An object type, encompassing the necessary information
-- that an object should have.
data Object = Object { objectMass :: Double, objectLocation :: Position, objectVelocity :: Velocity }
  deriving (Eq, Show)

instance (Num a, Num b) => Num (a,b) where
  (x1,x2) + (y1,y2) = (x1+y1,x2+y2)
  (x1,x2) * (y1,y2) = (x1*y1,x2*y2)
  negate (x,y) = (negate x, negate y)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

-- | Vector scaling function.
(|*|) :: Vector -> Double -> Vector
(|*|) v d = v * vector d d

-- | Vector constructor
vector :: Double -> Double -> Vector
vector a b = (a, b)

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

-- | Time for simulation.
type Time = Double

-- | Updates the position and velocity of an object given a time
-- frame and the force applied to it in that time frame.
updatePosition :: Time -> Force -> Object -> Object
updatePosition t f (Object m p v) = Object m (p + (newV |*| t)) newV
  where
    newV = v + (acc |*| t) 
    acc = f |*| (1/m)

-- How do I want to use it?
--updatePosition :: Object -> Force -> Time -> Object
-- Object `update` F1 `update` F2 `update` F3 
