{-# LANGUAGE ExistentialQuantification #-}
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

data AnyIntent = forall a. Intent a => AnyIntent a
data MultiIntent = MultiIntent { actions :: [AnyIntent], timings :: Int }

instance Intent MultiIntent where
  enactIntent m flyer pos vel
    | null as   = return m
    | success $ head as = return m { actions = (tail as) }
    | otherwise = do
        a <- enactIntent (head as) flyer pos vel
        milliSleep t
        return m { actions = (a : (tail as)) }
    where
      as  = actions m
      t   = timings m
  success = null . actions

instance Intent AnyIntent where
  enactIntent (AnyIntent i) flyer pos vel = 
    fmap AnyIntent $ enactIntent i flyer pos vel
  success (AnyIntent i) = success i

(<&>) :: (Intent a) => MultiIntent -> a -> MultiIntent
(<&>) m i = m { actions = ((AnyIntent i) : (actions m)) }

doAll :: MultiIntent
doAll = MultiIntent [] 0

keepDoing :: MultiIntent -> MultiIntent
keepDoing m = m { actions = (cycle $ actions m) }

withTiming :: Int -> MultiIntent -> MultiIntent
withTiming t m = m { timings = t }
