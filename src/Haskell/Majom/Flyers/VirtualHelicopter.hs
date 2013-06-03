-- | Module to fly a virtual, simulated helicopter. Will be used to 
-- --TODO Make this vary based on position
-- test AI interface.

module Majom.Flyers.VirtualHelicopter (
  -- * Types
  VirtualHelicopter(..),
  -- * Classes
  -- * Functions
  spawnVirtualHelicopter
    ) where

import Majom.Common
import Majom.Control.PID
import Majom.Flyers.Flyable
import Majom.Simulation.SimpleSim

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Map as Map
import System.Random

-- | A data type containing base information about a helicopter, such as 
-- the current options it holds and its current position
data VirtualHelicopter = 
  VirtualHelicopter 
  { getOptions :: TVar [(Option, Int)], 
    getPosition :: TVar Position, 
    getCurrentOptions :: TVar OptionMap, 
    getActive :: TVar Bool,
    getPID :: TVar PID}

-- | Spawns a virtual helicopter at (0,0)
spawnVirtualHelicopter :: IO VirtualHelicopter
spawnVirtualHelicopter = atomically $ VirtualHelicopter <$> (newTVar []) <*> (newTVar (Position (vector [0,0,0]) 0)) <*> (newTVar $ Map.fromList flyerInit) <*> (newTVar False) <*> (newTVar newPID)

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
    rand <- sequence [genErr, genErr, genErr]
    --return (pwr, (pos <+> (vector rand)))
    return (pwr, pos)
    where
      genErr :: IO Double
      genErr = getStdRandom (randomR (-0.0001,0.0001))
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
  isActive h = atomically $ readTVar (getActive h)
  setActive h b = atomically $ writeTVar (getActive h) b
  getController h = atomically $ readTVar (getPID h)
  setController h p = atomically $ writeTVar (getPID h) p 

-- | Runs the flying simulation
run h = do 
  let settings = setFloor (Just (vector [0,0,0])) defaultSettings
  --let settings = defaultSettings
  forceVar <- startSimulation settings (getPosition h) $ simpleObject $ Position (vector [0,0,0]) 0
  forever $ do
    options <- atomically $ do
      options <- readTVar optionsVal
      clearOptions optionsVal
      return options
    processOptions h forceVar options
    milliSleep 50
  where
    optionsVal = getOptions h
    clearOptions x = writeTVar x []
    milliSleep = threadDelay . (*) 1000

-- | A map mapping options to their values.
type OptionMap = Map.Map Option Int

-- | Using a mapping from options and their values to forces, and an option
-- map, combines all of the resulting force into one. Essentially converts
-- all of the orders into a final force on the helicopter. 
convertToForce :: (Double -> Option -> Int -> Force) -> Double -> OptionMap -> Force
convertToForce f o m = Map.fold (+) (vector [0, 0, 0]) $ Map.mapWithKey (f o) m

-- | A basic mapping of options to particular forces.
basicMap :: Double -> Option -> Int -> Force
basicMap orientation o v = 
  case o of 
    Yaw -> vector [0,0,0]
    --TODO Make this vary based on position
    Throttle -> vector [0,0.14,0] |*| (fromIntegral v)
    Pitch -> vector [sin rs,0,cos rs] |*| ((1.0/63)*(fromIntegral $ 63 - v))
    Correction -> vector [0,0,0] 
  where
    rs = radians orientation

-- | Takes a list of options and applies them to the model.
processOptions :: VirtualHelicopter -> TVar Force -> [(Option, Int)] -> IO ()
processOptions h forceVar options = do
  let oldOptionsVar = getCurrentOptions h
  oldOptions <- atomically $ readTVar oldOptionsVar
  pos <- atomically $ readTVar (getPosition h)
  let orientation = getFacing pos
  newOptions <- atomically $ do
    let newOptions = foldl process oldOptions options
    writeTVar (getPosition h) $ pos { getFacing = (getFacing pos) + ((360*((fromIntegral $ newOptions Map.! Yaw) - 63))/(30*63)) }
    writeTVar forceVar $ 
      convertToForce basicMap orientation newOptions
    return newOptions
  --TODO Do the twist
  atomically $ writeTVar oldOptionsVar newOptions
  where
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert
