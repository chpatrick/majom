-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
  runMonkeyWithHuman
  ) where

import Majom.Analysis.Model
import Majom.Analysis.Kalman
import Majom.Common
import Majom.Flyers.Flyable
import Majom.Control.GUI
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent
import Majom.Control.PID
import Majom.Lang.LoopWhile
import Majom.Flyers.Special.DuoCopter

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent

import Data.IORef

-- | State transformer for MonkeyBrain
type MonkeyBrainT = StateT Brain IO ()

-- | State executor for a monkey brain
execMonkeyBrainT :: MonkeyBrainT -> Brain -> IO Brain
execMonkeyBrainT mk k = execStateT mk k

-- | Brain that holds all the relevant data for the monkey brain.
data Brain = Brain { brainIntent :: HoverIntent,
                     brainLast :: (Position, Power) }

desiredPos :: Vector
desiredPos = vector [0, 0.1, -1.5]

base :: Power
base = 80

-- | Starts the monkey (starts the flyer too)
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  forkIO $ fly flyer
  --putStrLn "I am a monkey"

  runMonkey' flyer

-- | Runs the monkey (internal function).
runMonkey' :: (Flyable a) => a -> IO Brain
runMonkey' flyer = do
  let intent = hoverAt $ Position desiredPos undefined

  milliSleep waitTime
  (_, pos) <- observe flyer

  setFly flyer Throttle 0
  setFly flyer Yaw 63
  posRef <- newIORef (pos, 0)

  milliSleep waitTime
  (_, p') <- observe flyer
  milliSleep waitTime
  let initBrain = Brain intent (p', 0)
  setFly flyer Pitch 48
  execMonkeyBrainT (forever $ monkeyDo flyer) initBrain

-- | Lets the human fly the flyer with the monkey, controlling a specific
-- set of options.
runMonkeyWithHuman :: (Flyable a) => [Option] -> a -> IO Brain
runMonkeyWithHuman humanControl flyer = do
  let (human, monkey) = startDuoCopter flyer humanControl
  forkIO $ runGUI human
  putStrLn "I am a monkey"
  runMonkey' monkey

-- The last thing is the return value
-- | The iterative loop of the monkey.
monkeyDo :: (Flyable a) => a -> MonkeyBrainT
monkeyDo flyer = do
  obs@(pwr', pos') <- lift $ observe flyer
  active <- lift $ isActive flyer
  if active 
    then do
      (Brain intent (pos, pwr)) <- get
      pid <- lift $ getController flyer

      lift $ putStrLn $ prettyPos pos'

      let err = (vectorY desiredPos) - (vectorY $ getVec pos')
      let (pid', m) = getMV pid err

      lift $ putStrLn $ show (base + floor m)

      let vel = getVec (pos' - pos) |/| wt
      let fwds = vectorSize $ getVel intent pos'
      put $ Brain intent (pos, pwr)

      lift $ setController flyer pid'
      lift $ setFly flyer Pitch $ 63 - floor (15 * fwds)
      lift $ setFly flyer Throttle $ base + floor m
      lift $ setFly flyer Yaw $ getYaw (getHeading intent pos') pos' 
    else do
      iters <- lift $ fmap length $ 
        takeWhileIO (not . id) $
          repeat (milliSleep waitTime >> observe flyer >> isActive flyer)
      (Brain intent (pos,  _)) <- get
      obs@(pwr, pos') <- lift $ observe flyer
      let vel' = (getVec (pos' - pos)) |/| (wt * (fromIntegral iters))
      put $ Brain intent (pos',pwr)

  lift $ milliSleep waitTime

takeWhileIO :: (a -> Bool) -> [IO a] -> IO [a]
takeWhileIO _ [] = return []
takeWhileIO p (ix:ixs) = do
  x <- ix 
  if p x 
    then
      (return . (x:)) =<< takeWhileIO p ixs
    else 
      return []

getYaw :: Vector -> Position -> Int
getYaw v pos =
  if abs (degDiff o o') > 10 
    then 
        floor $ 63 + (signum (degDiff o o'))*20
    else 63
  where
    vec = vectorUnit v
    o = degNorm $ getFacing pos
    o' = 
      if vectorX vec >= 0 && vectorZ vec >= 0 then base
      else if vectorX vec >= 0 then 180 + base
      else if vectorZ vec >= 0 then 360 + base
      else 180 + base
    base = degrees $ atan $ (vectorX vec) / (vectorZ vec)

-- | The iteration time of the monkey (milliseconds)
waitTime :: Int
waitTime = 50

-- | Wait time in seconds.
wt :: Double
wt = (fromIntegral waitTime) / 1000.0

-- | Sleeps the monkey for given milliseconds.
milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
