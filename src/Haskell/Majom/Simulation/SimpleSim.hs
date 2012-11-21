-- | A module for performing simple physics simulations. 
-- Will be useful for testing helicopter controls using an
-- abstracted environment.
module Majom.Simulation.SimpleSim (
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
  (|*|),
  updatePosition,
  simpleObject,
  startSimulation
  ) where

import Control.Concurrent.STM
import Control.Concurrent

-- | An object type, encompassing the necessary information
-- that an object should have.
data Object = Object { objectMass :: Double, objectLocation :: Position, objectVelocity :: Velocity }
  deriving (Eq, Show)

simpleObject :: Position -> Object
simpleObject p = Object 1 p 0

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
vector x y = (x, y)

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

-- | Gravity force constant
gravity :: Force
gravity = vector 0 (-9.8)

--foo :: [(Force, (Time,Time))] -> Object -> Object

-- | Updates the position and velocity of an object given a time
-- frame and the forces applied to it in that time frame.
updatePosition :: Time -> Force -> Object -> Object
updatePosition t fs (Object m p v) =
  Object m (p + (v |*| t) + (acc |*| (0.5 * (t^2) ) ) ) newV
  where
    newV = v + (acc |*| t) 
    acc = fs |*| (1/m)

-- | Starts a simulation thread, showing the simulation on a GUI
startSimulation :: TVar Position -> Object -> IO (TVar Force)
startSimulation position object = do
  forces <- atomically $ newTVar (vector 0 0)
  forkIO $ simulate forces position object
  return forces

-- | Iteration time in milliseconds
stepTime :: Int
stepTime = 1000

-- | Converts milliseconds to seconds
milliToSeconds :: Int -> Double
milliToSeconds = (/1000) . fromIntegral

simulate :: TVar Force -> TVar Position -> Object -> IO ()
simulate forceVar positionVar object = do
    displayObject object
    force <- atomically $ readTVar forceVar
    atomically $ writeTVar positionVar $ objectLocation object
    threadDelay (stepTime * 1000)
    simulate forceVar positionVar $ 
      updatePosition (milliToSeconds stepTime) (force + gravity) object

displayObject :: Object -> IO ()
displayObject o = putStrLn $ "Loc: " ++ (show $ objectLocation o) ++ ", Vel: " ++ (show $ objectVelocity o)
