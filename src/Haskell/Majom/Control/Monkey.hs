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
  let intent = hoverAt (vector [50,100,0])
  forkIO $ fly flyer
  milliSleep waitTime
  (_, pos, _) <- observe flyer
  milliSleep waitTime
  (_, pos', _) <- observe flyer
  let vel = (pos - pos') |/| wt
  let initBrain = Brain createNewModel intent (pos', vel, undefined)
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

monkeySay :: (Model a) => 
  (a, a, Power, Position, Velocity, Power, Acceleration) -> IO ()
monkeySay (model, model', pwr, pos', vel', pwr', accel') = do
  putStrLn $ (show $ length $ samples model') ++ " samples."
  putStrLn $ "Observed pwr: " ++ (show pwr)
  putStrLn $ "New position: " ++ (show pos')
  putStrLn $ "New velocity: " ++ (show vel')
  putStrLn $ "Setting fly to " ++ (show pwr') ++ " for accel " ++ (show accel')
  putStrLn $ show $ samples model
  putStrLn ""
  
monkeyThink :: (Intent a, Model b) => 
  a -> b -> Power -> (Position, Position) -> Velocity 
  -> (b, Position, Velocity, Power)
monkeyThink intent model pwr (pos, pos') vel = do
  (model', pos', vel', pwr')
  where
    vel' = (pos' - pos) |/| wt
    accel = (vel' - vel) |/| wt
    model' = updateModel model (pwr, accel)
    pwr' = (getMap model') $ getAccel intent vel' pos

-- The last thing is the return value
monkeyDo :: (Flyable a) => a -> MonkeyBrainT
monkeyDo flyer = do
  lift $ milliSleep waitTime

  (Brain model intent (pos, vel, _)) <- get
  obs@(pwr, pos', _) <- lift $ observe flyer
  let (model', pos', vel', pwr') = monkeyThink intent model pwr (pos, pos') vel

  -- For debugging
  lift $ monkeySay (model, model', pwr, pos', vel', pwr', getAccel intent vel' pos)

  put $ Brain model' intent (pos', vel', undefined)
  lift $ setFly flyer Throttle pwr'


waitTime :: Int
waitTime = 100

wt :: Double
wt = (fromIntegral waitTime) / 1000.0

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
