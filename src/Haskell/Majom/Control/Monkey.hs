-- | Bot to control the flyer.
module Majom.Control.Monkey (
  ) where

-- The idea is that the controller has no idea exactly how much force is exerted
-- from the helicopter for the inputs, it just knows that the function is 
-- monotonically increasing and continuous. It has to learn the function from
-- observing the helicopter.

-- What it has to work with:
-- 1 - It can observe the helicopter at any time
-- 2 - Power -> Acceleration is monotonic increasing
-- 3 - Power -> Acceleration is continuous
-- 4 - Simple case, environment forces are constant
--   - Complex case, environment forces are not constant
--
-- Needs to be able to iteratively come to an approximation of the 
-- Power -> Acceleration function, changing as new input comes in (adaptive).

import Majom.Analysis.Model
import Majom.Common
import Majom.Flyers.Flyable

import Control.Monad

data Intention = Intention -- To be defined properly

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO ()
runMonkey flyer = do
  let model = createNewModel
  forever $ do 
    pos <- observe flyer
    return ()
    --monkeyDo undefined model pos

-- | Handles observing the flyer, updating the model, and sending commands.
--monkeyDo :: Monkey a -> Intention -> Model -> Position -> Monkey a
--monkeyDo = undefined
