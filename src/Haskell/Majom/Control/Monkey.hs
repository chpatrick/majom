-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
  runMonkeyWithHuman
  ) where

import Majom.Analysis.Model
import Majom.Analysis.LeastSquares
import Majom.Common
import Majom.Flyers.Flyable
import Majom.Control.GUI
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent
import Majom.Lang.LoopWhile
import Majom.Flyers.Special.DuoCopter

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent

import Data.IORef

import System.CPUTime

type MonkeyBrainT = StateT Brain IO ()

execMonkeyBrainT :: MonkeyBrainT -> Brain -> IO Brain
execMonkeyBrainT mk k = execStateT mk k

data Brain = Brain { brainModel :: LeastSquares,
                     brainIntent :: HoverIntent,
                     brainLast :: (Position, Velocity, FlyState) }

data FlyState = Flying | Landed deriving (Show, Eq)

-- | Starts the monkey
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  forkIO $ fly flyer

  runMonkey' flyer

runMonkey' :: (Flyable a) => a -> IO Brain
runMonkey' flyer = do
  let intent = hoverAt (vector [50,51,0])

  milliSleep waitTime
  (_, pos, _) <- observe flyer

  setFly flyer Throttle 0
  posRef <- newIORef (pos, 0)

  milliSleep waitTime

  -- For sake of experiment, let's assume that initially its landed.
  loop $ do (pos,throttle) <- lift $ readIORef posRef
            (_,pos',_) <- lift $ observe flyer
            let vel = (pos - pos')
            lift $ writeIORef posRef (pos', throttle + 2)
            while (vectorSize vel == 0.0)
            lift $ setFly flyer Throttle (throttle + 2)
            lift $ putStrLn $ "Taking off... new throttle = " ++ (show $ throttle + 2)
            lift $ milliSleep waitTime
            
  (pos',_) <- readIORef posRef
  milliSleep waitTime
  (_, pos'', _) <- observe flyer
  let vel = (pos'' - pos) |/| wt
  let initBrain = Brain createNewModel intent (pos', vel, undefined)
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

-- | Lets the human fly the flyer with the monkey, controlling a specific
-- set of options.
runMonkeyWithHuman :: (Flyable a) => [Option] -> a -> IO Brain
runMonkeyWithHuman humanControl flyer = do
  let (human, monkey) = startDuoCopter flyer humanControl
  forkIO $ runGUI human
  runMonkey' monkey

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
  -> (b, Velocity, Power)
monkeyThink intent model pwr (pos, pos') vel = do
  (model', vel', pwr')
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
  let (model', vel', pwr') = monkeyThink intent model pwr (pos, pos') vel

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
