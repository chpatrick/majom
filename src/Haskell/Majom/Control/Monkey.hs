-- | Bot to control the flyer.
module Majom.Control.Monkey (
  ) where

-- The idea is that the controller has no idea exactly how much force is exerted
-- from the helicopter for the inputs, it just knows that the function is 
-- monotonically increasing and continuous. It has to learn the function from
-- observing the helicopter.
