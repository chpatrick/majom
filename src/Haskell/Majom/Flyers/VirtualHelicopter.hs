-- | Module to fly a virtual, simulated helicopter. Will be used to 
-- test AI interface.

module Majom.Flyers.VirtualHelicopter (
  ) where

import Majom.Flyers.Flyable

data VirtualHelicopter = VirtualHelicopter

instance Flyable VirtualHelicopter where
  setFly = undefined
  setFlyMany = undefined


-- Need a seperate thread that ticks every n seconds, evaluating
-- the change of force that I apply to the helicopter and updating
-- the simulation.

-- Send messages -> Thread -> (FrontEnd that maps 1-127 to some continuous function that corresponds to force) -> Simulation
--
