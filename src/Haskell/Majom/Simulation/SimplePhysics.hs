module Majom.Simulation.SimplePhysics where

data Object = Object { objectMass :: Double, objectLocation :: Position, objectVelocity :: Velocity }
  deriving (Eq, Show)

instance (Num a, Num b) => Num (a,b) where
  (x1,x2) + (y1,y2) = (x1+y1,x2+y2)
  (x1,x2) * (y1,y2) = (x1*y1,x2*y2)
  negate (x,y) = (negate x, negate y)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger i = (fromInteger i, fromInteger i)

(|*|) :: Vector -> Double -> Vector
(|*|) v d = v * vector d d

vector :: Double -> Double -> Vector
vector a b = (a, b)

type Vector = (Double, Double)

type Position = Vector
type Velocity = Vector
type Acceleration = Vector
type Force = Vector

type Time = Double

updatePosition :: Time -> Force -> Object -> Object
updatePosition t f (Object m p v) = Object m (p + (newV |*| t)) newV
  where
    newV = v + (acc |*| t) 
    acc = f |*| (1/m)

-- How do I want to use it?
updatePosition :: Object -> Force -> Time -> Object
-- Object `update` F1 `update` F2 `update` F3 
