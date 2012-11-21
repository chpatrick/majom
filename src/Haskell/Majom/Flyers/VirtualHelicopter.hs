-- | Module to fly a virtual, simulated helicopter. Will be used to 
-- test AI interface.

module Majom.Flyers.VirtualHelicopter (
  -- * Types
  VirtualHelicopter(..),
  -- * Classes
  -- * Functions
  spawnVirtualHelicopter
    ) where

import Majom.Flyers.Flyable
import Majom.Simulation.SimpleSim
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as Map

data VirtualHelicopter = VirtualHelicopter { getOptions :: TVar [(Option, Int)] }

spawnVirtualHelicopter :: IO VirtualHelicopter
spawnVirtualHelicopter = atomically $ fmap VirtualHelicopter (newTVar [])

instance Flyable VirtualHelicopter where
  setFly h o v = do
    let options = getOptions h
    atomically $ writeTVar options . ((o, v) :) =<< readTVar options
    return ()
  setFlyMany h vs = do 
    let options = getOptions h
    atomically $ writeTVar options . (vs ++) =<< readTVar options
    return ()
  fly = run 
  observe = undefined


-- Need a seperate thread that ticks every n seconds, evaluating
-- the change of force that I apply to the helicopter and updating
-- the simulation.

-- Send messages -> Thread -> (FrontEnd that maps 1-127 to some continuous function that corresponds to force) -> Simulation

run h = do 
  forceVar <- startSimulation $ simpleObject $ vector 50 50
  oldOptionsVar <- newIORef $ Map.empty
  forever $ do
    options <- atomically $ do
      options <- readTVar optionsVal
      clearOptions optionsVal
      return options
    processOptions oldOptionsVar forceVar options
    milliSleep 120
  where
    optionsVal = getOptions h
    clearOptions x = writeTVar x []
    milliSleep = threadDelay . (*) 1000

type OptionMap = Map.Map Option Int

convertToForce :: (Option -> Int -> Force) -> OptionMap -> Force
convertToForce f m = Map.fold (+) (vector 0 0) $ Map.mapWithKey f m

basicMap :: Option -> Int -> Force
basicMap o v = 
  case o of 
    Yaw -> vector 0 0
    Pitch -> vector 0 0 
    Throttle -> (vector 0 0.2) |*| (fromIntegral v)
    Correction -> vector 0 0 

-- | Takes a list of options and applies them to the model.
processOptions :: IORef OptionMap -> TVar Force -> [(Option, Int)] -> IO ()
processOptions oldOptionsVar forceVar options = do
  oldOptions <- readIORef oldOptionsVar
  atomically $ 
    writeTVar forceVar $ 
      convertToForce basicMap $ foldl process oldOptions options
  where
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert
