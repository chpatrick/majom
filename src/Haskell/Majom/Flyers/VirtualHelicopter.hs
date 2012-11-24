-- | Module to fly a virtual, simulated helicopter. Will be used to 
-- test AI interface.

module Majom.Flyers.VirtualHelicopter (
  -- * Types
  VirtualHelicopter(..),
  -- * Classes
  -- * Functions
  spawnVirtualHelicopter
    ) where

import Majom.Common
import Majom.Flyers.Flyable
import Majom.Simulation.SimpleSim

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as Map

-- | A data type containing base information about a helicopter, such as 
-- the current options it holds and its current position
data VirtualHelicopter = VirtualHelicopter { getOptions :: TVar [(Option, Int)], getPosition :: TVar Position }

-- | Spawns a virtual helicopter at (0,0)
spawnVirtualHelicopter :: IO VirtualHelicopter
spawnVirtualHelicopter = atomically $ VirtualHelicopter <$> (newTVar []) <*> (newTVar (vector2 0 0))

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
  observe h = atomically $ readTVar $ getPosition h

-- | Runs the flying simulation
run h = do 
  forceVar <- startSimulation (getPosition h) $ simpleObject $ vector2 50 50
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

-- | A map mapping options to their values.
type OptionMap = Map.Map Option Int

-- | Using a mapping from options and their values to forces, and an option
-- map, combines all of the resulting force into one. Essentially converts
-- all of the orders into a final force on the helicopter. 
convertToForce :: (Option -> Int -> Force) -> OptionMap -> Force
convertToForce f m = Map.fold (+) (vector2 0 0) $ Map.mapWithKey f m

-- | A basic mapping of options to particular forces.
basicMap :: Option -> Int -> Force
basicMap o v = 
  case o of 
    Yaw -> vector2 0 0
    Pitch -> vector2 0 0 
    Throttle -> (vector2 0 0.2) |*| (fromIntegral v)
    Correction -> vector2 0 0 

-- | Takes a list of options and applies them to the model.
processOptions :: IORef OptionMap -> TVar Force -> [(Option, Int)] -> IO ()
processOptions oldOptionsVar forceVar options = do
  oldOptions <- readIORef oldOptionsVar
  newOptions <-
    atomically $ do 
      let newOptions = foldl process oldOptions options
      writeTVar forceVar $ 
        convertToForce basicMap newOptions
      return newOptions
  writeIORef oldOptionsVar newOptions

  where
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert
