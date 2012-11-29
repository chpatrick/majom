-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
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
import Control.Monad.State
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time.Clock

data Intention = Intention -- To be defined properly

-- TODO Modularise this so that I can plug it in and out as
-- necessary. Need to get both Power and Position, then calculate
-- the acceleration and apply the power to it.

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO ()
runMonkey flyer = do
  forkIO $ fly flyer
  milliSleep 100
  -- Watch the GUI
  -- Need a way to retrieve power values...
  -- guiID <- forkIO $ runGUIManualFly flyer
  --(_,pos) <- runStateT (replicateM 20 (update flyer)) []
  -- killThread guiID
  --putStrLn $ show pos

-- | Converts observed positions at times into perceived acceleration changes.
-- | I don't reallllly understand fully what is going on here.
-- I know that it's cool though. 
type PositionState = StateT [Position] IO
update :: Flyable a => a -> PositionState ()
update flyer = do
  ps <- get
  p <- liftIO $ observe flyer
  liftIO $ milliSleep 100
  put (p:ps)

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
