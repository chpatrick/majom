-- | A module for performing simple physics simulations. 
-- Will be useful for testing helicopter controls using an
-- abstracted environment.
module Majom.Simulation.SimpleSim (
  -- * Types
  Object(..),
  -- * Functions
  updatePosition,
  simpleObject,
  startSimulation
  ) where

import Control.Concurrent.STM
import Control.Concurrent

import Majom.Common

-- | An object type, encompassing the necessary information
-- that an object should have.
data Object = Object { objectMass :: Double, objectLocation :: Position, objectVelocity :: Velocity }
  deriving (Eq, Show)

simpleObject :: Position -> Object
simpleObject p = Object 1 p 0

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
  forces <- atomically $ newTVar (vector2 0 0)
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
