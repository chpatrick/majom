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
data VirtualHelicopter = VirtualHelicopter { getOptions :: TVar [(Option, Int)], getPosition :: TVar Position, getCurrentOptions :: TVar OptionMap }

-- | Spawns a virtual helicopter at (0,0)
spawnVirtualHelicopter :: IO VirtualHelicopter
spawnVirtualHelicopter = atomically $ VirtualHelicopter <$> (newTVar []) <*> (newTVar (vector [0,0,0])) <*> (newTVar Map.empty)

clamp :: Int -> Int
clamp i
  | i > 127   = 127
  | i < 0     = 0
  | otherwise = i

instance Flyable VirtualHelicopter where
  setFly h o v = do
    let options = getOptions h
    atomically $ writeTVar options . ((o, clamp v) :) =<< readTVar options
    return ()
  setFlyMany h vs = do 
    let options = getOptions h
    atomically $ writeTVar options . (vs ++) =<< readTVar options
    return ()
  fly = run 
  observe h = do
    (pwr, pos) <- atomically $ do
      pos <- readTVar $ getPosition h
      pwr <- currentPower
      return (pwr, pos)
    return (pwr, pos)
    where
      sequence3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
      sequence3 (m1, m2, m3) = do
        x1 <- m1;
        x2 <- m2;
        x3 <- m3;
        return (x1, x2, x3)
      currentPower = do
        optMap <- readTVar $ getCurrentOptions h
        return $ if Map.member Throttle optMap then
          optMap Map.! Throttle else
          0

-- | Runs the flying simulation
run h = do 
  let settings = setFloor (Just (vector [0,0,0])) defaultSettings
  --let settings = defaultSettings
  forceVar <- startSimulation settings (getPosition h) $ simpleObject $ vector [0, 0, 0]
  forever $ do
    options <- atomically $ do
      options <- readTVar optionsVal
      clearOptions optionsVal
      return options
    processOptions (getCurrentOptions h) forceVar options
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
convertToForce f m = Map.fold (+) (vector [0, 0, 0]) $ Map.mapWithKey f m

-- | A basic mapping of options to particular forces.
basicMap :: Option -> Int -> Force
basicMap o v = 
  case o of 
    Yaw -> vector [0,0,0]
    Pitch -> vector [0.1/63,0,0] |*| (fromIntegral $ v - 63)
    Throttle -> vector [0,0.143,0] |*| (fromIntegral v)
    Correction -> vector [0,0,0] 

-- | Takes a list of options and applies them to the model.
processOptions :: TVar OptionMap -> TVar Force -> [(Option, Int)] -> IO ()
processOptions oldOptionsVar forceVar options = do
  oldOptions <- atomically $ readTVar oldOptionsVar
  newOptions <- atomically $ do
    let newOptions = foldl process oldOptions options
    writeTVar forceVar $ 
      convertToForce basicMap newOptions
    return newOptions
  atomically $ writeTVar oldOptionsVar newOptions

  where
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert
