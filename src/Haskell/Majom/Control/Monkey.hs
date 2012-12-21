-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
  ) where

import Majom.Analysis.Model
import Majom.Analysis.LeastSquares
import Majom.Common
import Majom.Flyers.Flyable
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent

import System.CPUTime

type MonkeyBrainT = StateT Brain IO ()

execMonkeyBrainT :: MonkeyBrainT -> Brain -> IO Brain
execMonkeyBrainT mk k = execStateT mk k

data Brain = Brain { brainModel :: LeastSquares,
                     brainIntent :: HoverIntent,
                     brainLast :: (Position, Velocity, Time) }

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  let intent = hoverAt (vector [50,100])
  forkIO $ fly flyer
  milliSleep waitTime
  (_, pos, t) <- observe flyer
  milliSleep waitTime
  (_, pos', t') <- observe flyer
  let vel = (pos - pos') |/| wt
  let initBrain = Brain createNewModel intent (pos', vel, t')
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

-- The last thing is the return value
monkeyDo :: (Flyable a) => a -> MonkeyBrainT
monkeyDo flyer = do
  lift $ milliSleep waitTime
  (Brain model intent (pos, vel, t)) <- get
  obs@(pwr, pos', t') <- lift $ observe flyer
  let vel' = (pos' - pos) |/| wt --(t' - t)
  let accel = (vel' - vel) |/| wt --(t' - t)
  let model' = updateModel model (pwr, accel)
  let pwr' = (getMap model') $ getAccel intent vel' pos
--  lift $ if isSeeded model' then putStrLn "Seeded!" else putStrLn "Not seeded :("
  lift $ putStrLn $ (show $ samples model') ++ " samples."
  lift $ putStrLn $ "Observed pwr: " ++ (show pwr)
  lift $ putStrLn $ "New position: " ++ (show pos')
  lift $ putStrLn $ "New velocity: " ++ (show vel')
  lift $ putStrLn $ "New acceleration: " ++ (show accel)
  lift $ putStrLn $ "Setting fly to " ++ (show pwr') ++ " for accel " ++ (show $ getAccel intent vel' pos)
  --lift $ putStrLn $ show $ lsSamples model'
  lift $ putStrLn ""
  put $ Brain model' intent (pos', vel', t')
  lift $ setFly flyer Throttle pwr'

waitTime :: Int
waitTime = 100

wt :: Double
wt = (fromIntegral waitTime) / 1000.0

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
