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

import Majom.Flyers.Helicopter

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


-- Need a seperate thread that ticks every n seconds, evaluating
-- the change of force that I apply to the helicopter and updating
-- the simulation.

-- Send messages -> Thread -> (FrontEnd that maps 1-127 to some continuous function that corresponds to force) -> Simulation

run h = do
  forever ( (putStrLn . show =<< (atomRead orders))
            >> (setFlyMany Helicopter =<< (atomRead orders))
            >> clearOrders orders
            >> milliSleep 2000)
  where
    orders = getOrders h
    clearOrders x = atomically $ writeTVar x []
    addOrder v xs = atomically $ writeTVar xs . (v:) =<< readTVar xs
    milliSleep = threadDelay . (*) 1000
    appV fn x = atomically $ writeTVar x . fn =<< readTVar x
    atomRead = atomically . readTVar
    dispVar x = atomRead x >>= print
