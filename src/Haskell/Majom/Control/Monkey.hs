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
import Majom.Control.Monkey.Intention

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Data.Time.Clock

type MonkeyBrainT = StateT Brain IO ()

execMonkeyBrainT :: MonkeyBrainT -> Brain -> IO Brain
execMonkeyBrainT mk k = execStateT mk k

data Brain = Brain { brainModel :: Kalman,
                     brainIntent :: Intention,
                     brainLast :: (Position, Velocity, UTCTime) }

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  let intent = undefined
  forkIO $ fly flyer
  (_, pos, t) <- observe flyer
  milliSleep waitTime
  (_, pos', t') <- observe flyer
  let vel = (pos - pos') |/| (diffTime t' t)
  let initBrain = Brain createNewModel intent (pos', vel, t')
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

-- The last thing is the return value
monkeyDo :: (Flyable a) => a -> MonkeyBrainT
monkeyDo flyer = do
  (Brain model intent (pos, vel, t)) <- get
  obs@(pwr, pos', t') <- lift $ observe flyer
  let vel' = (pos - pos') |/| (diffTime t' t)
  let accel = (vel' - vel) |/| (diffTime t' t)
  let model' = updateModel model (pwr, accel)
  let pwr' = (getMap model') $ getAccel intent vel' pos
  put $ Brain model' intent (pos', vel', t')
  lift $ setFly flyer Throttle pwr'
  lift $ milliSleep waitTime

waitTime :: Int
waitTime = 100

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
