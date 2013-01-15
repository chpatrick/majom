-- | A module for performing simple physics simulations. 
-- Will be useful for testing helicopter controls using an
-- abstracted environment.
module Majom.Simulation.SimpleSim (
  -- * Types
  Object(..),
  -- * Functions
  updatePosition,
  simpleObject,
  defaultSettings,
  setFloor,
  startSimulation
  ) where

import Majom.Common
import Control.Concurrent.STM
import Control.Concurrent

-- | An object type, encompassing the necessary information
-- that an object should have.
data Object = Object { objectMass :: Double, objectLocation :: Position, objectVelocity :: Velocity }
  deriving (Eq, Show)

-- | Constructs a basic point mass at a given position.
simpleObject :: Position -> Object
simpleObject p = Object 1 p (vector [0,0,0])

-- | Updates the position and velocity of an object given a time
-- frame and the forces applied to it in that time frame.
updatePosition :: Time -> Force -> Object -> Object
updatePosition t fs (Object m p v) =
  Object m (p + (v |*| t) + (acc |*| (0.5 * (t^2) ) ) ) newV
  where
    newV = v + (acc |*| t) 
    acc = fs |*| (1/m)

data SimulationSettings = Settings { simFloor :: Maybe Position } 
  deriving Show

defaultSettings :: SimulationSettings
defaultSettings = Settings Nothing

setFloor :: Maybe Position -> SimulationSettings -> SimulationSettings
setFloor p s = s{simFloor = p}

-- | Starts a simulation thread, showing the simulation on a GUI
startSimulation :: SimulationSettings -> TVar Position -> Object -> IO (TVar Force)
startSimulation settings position object = do
  forces <- atomically $ newTVar (vector [0, 0, 0])
  forkIO $ simulate settings forces position object
  return forces

-- | Iteration time in milliseconds
stepTime :: Int
stepTime = 50

-- | Converts milliseconds to seconds
milliToSeconds :: Int -> Double
milliToSeconds = (/1000) . fromIntegral

simulate :: SimulationSettings -> TVar Force -> TVar Position -> Object -> IO ()
simulate settings forceVar positionVar object = do
    displayObject object
    force <- atomically $ readTVar forceVar
    atomically $ writeTVar positionVar $ objectLocation object
    threadDelay (stepTime * 1000)
    let obj' = updatePosition (milliToSeconds stepTime) (force + gravity) object
    let pos' = objectLocation obj'
    let vel' = objectVelocity obj'
    simulate settings forceVar positionVar $ 
      -- | Check case of floor - could be extracted
      case simFloor settings of
        Nothing -> obj'
        Just f -> if objectLocation obj' `lowerThan` f
          then
            obj'{objectLocation = vector [vectorX pos', vectorY f, vectorZ pos'],objectVelocity = vector [vectorX vel', 0.0, vectorZ vel'] }
          else
            obj'

-- | Returns true if the second var is lower (has a lesser Y vector)
-- than the first var.
lowerThan :: Position -> Position -> Bool
lowerThan p1 p2 = vectorY p1 < vectorY p2

displayObject :: Object -> IO ()
displayObject o = putStrLn $ "Loc: " ++ (show $ objectLocation o) ++ ", Vel: " ++ (show $ objectVelocity o)
