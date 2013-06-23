{-# LANGUAGE ExistentialQuantification #-}
-- | Module for chaining multiple basic intents together
module Majom.Control.Monkey.MultiIntent (
  MultiIntent,
  (<&>),
  doAll,
  keepDoing,
  withTiming
  ) where

import Majom.Common
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent
import Majom.Control.Monkey.LandIntent
import Majom.Flyers.Flyable

import Control.Concurrent

-- | Coverall structure for any intent
data AnyIntent = forall a. Intent a => AnyIntent a
-- | MultiIntent consisting of its individual intents and the timings (desired time, wait time between iters)
data MultiIntent = MultiIntent { actions :: [AnyIntent], timings :: (Int, Int) }

instance Intent MultiIntent where
  enactIntent m flyer pos vel
    | null as   = return m
    | success $ head as = do
        milliSleep t
        return m { actions = (tail as) }
    | otherwise = do
        a <- enactIntent (head as) flyer pos vel
        return m { actions = (a : (tail as)) }
    where
      as  = actions m
      (t, wt) = timings m
  success = null . actions

instance Intent AnyIntent where
  enactIntent (AnyIntent i) flyer pos vel = 
    fmap AnyIntent $ enactIntent i flyer pos vel
  success (AnyIntent i) = success i

-- | Concats two multiintents.
(<&>) :: (Intent a) => MultiIntent -> a -> MultiIntent
(<&>) m i = m { actions = (actions m) ++ [AnyIntent i] }

-- | Base multiintent
doAll :: MultiIntent
doAll = MultiIntent [] 0

-- | Repeats all of the given intents forever
keepDoing :: MultiIntent -> MultiIntent
keepDoing m = m { actions = (cycle $ actions m) }

-- | Adds a timing to a multiintent
withTiming :: (Int, Int) -> MultiIntent -> MultiIntent
withTiming t m = m { timings = t }
