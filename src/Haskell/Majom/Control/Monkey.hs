-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
  ) where

import Majom.Analysis.Model
import Majom.Analysis.Kalman
import Majom.Common
import Majom.Flyers.Flyable

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Data.Time.Clock

data Intention = Intention -- To be defined properly

getAccel :: Intention -> Velocity -> Position -> Power
getAccel = undefined

type MonkeyBrainT = StateT Brain IO ()

execMonkeyBrainT :: MonkeyBrainT -> Brain -> IO Brain
execMonkeyBrainT mk k = execStateT mk k

data Brain = Brain { brainModel :: Kalman,
                     brainIntent :: Intention,
                     brainLast :: (Position, Velocity, UTCTime) }

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  forkIO $ fly flyer
  let initBrain = undefined -- set up the model and stuff here
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

-- The last thing is the return value
monkeyDo :: (Flyable a) => a -> MonkeyBrainT
monkeyDo flyer = do
  (Brain model intent (pos, vel, t)) <- get
  obs@(pwr, pos', t') <- lift $ observe flyer
  let vel' = (pos - pos') |/| (diffTime t' t)
  let accel = (vel' - vel) |/| (diffTime t' t)
  let model' = updateModel model (pwr, accel)
  let pwr' = getAccel intent vel' pos
  put $ Brain model' intent (pos', vel', t')
  lift $ setFly flyer Throttle pwr'
  lift $ milliSleep waitTime

waitTime :: Int
waitTime = 100

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
