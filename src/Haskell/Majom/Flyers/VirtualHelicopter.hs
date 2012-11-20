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
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Majom.Simulation.SimpleSim

data VirtualHelicopter = VirtualHelicopter { getOrders :: TVar [(Option, Int)] }

spawnVirtualHelicopter :: IO VirtualHelicopter
spawnVirtualHelicopter = atomically $ fmap VirtualHelicopter (newTVar [])

instance Flyable VirtualHelicopter where
  setFly h o v = do
    let orders = getOrders h
    atomically $ writeTVar orders . ((o,v) :) =<< readTVar orders
    return ()
  setFlyMany h vs = do 
    let orders = getOrders h
    atomically $ writeTVar orders . (vs ++) =<< readTVar orders
    return ()
  fly = run 
  observe = undefined


-- Need a seperate thread that ticks every n seconds, evaluating
-- the change of force that I apply to the helicopter and updating
-- the simulation.

-- Send messages -> Thread -> (FrontEnd that maps 1-127 to some continuous function that corresponds to force) -> Simulation

run h = do 
  shared <- startSimulation undefined -- + simulation params
  forever $ do
    atomically $ do
      return . processOrders shared =<< readTVar orders
      clearOrders orders
    milliSleep 120
  where
    orders = getOrders h
    clearOrders x = writeTVar x []
    milliSleep = threadDelay . (*) 1000

-- | Takes a list of orders and applies them to the model.
processOrders :: TVar [Force] -> [(Option, Int)] -> ()
processOrders = undefined
  where
    convert :: (Option, Int) -> Force
    convert = undefined
