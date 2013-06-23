-- | Bot to control the flyer.
module Majom.Control.Monkey (
  -- * Classes
  -- * Types
  -- * Functions
  runMonkey,
  runMonkeyWithHuman
  ) where

import Majom.Common
import Majom.Flyers.Flyable
import Majom.Control.GUI
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent
import Majom.Control.Monkey.LandIntent
import Majom.Control.Monkey.MultiIntent
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
data Brain = Brain { 
                     brainIntent :: MultiIntent,
                     brainLast :: (Position, Power) }

desiredPos :: Vector
desiredPos = vector [0.1, 0.2, -1.7]

-- | Starts the monkey (starts te flyer too)
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  forkIO $ fly flyer

  runMonkey' flyer

-- | Runs the monkey (internal function).
runMonkey' :: (Flyable a) => a -> IO Brain
runMonkey' flyer = do
  {-

  putStrLn "Please place the helicopter where you wish the landing position to be"
  takeWhileIO not $
    repeat (fmap (== '\n') getChar)
  (_,landPos) <- observe flyer
  -- Do Stuff
  putStrLn "Please clear the flying area!"
  takeWhileIO not $
    repeat (fmap (== '\n') getChar)
  putStrLn "Let's gooooo!"
  milliSleep 1000
  --putStrLn $ show (filter sSpecial surfaces)
  -}

  let hIntent = hoverAt $ Position desiredPos undefined

  --let lIntent = landOn $ landPos
  --let intent = keepDoing $ withTiming (1000, waitTime)  $ doAll <&> hIntent <&> lIntent
  let intent = keepDoing $ doAll <&> hIntent

  milliSleep waitTime
  (_, pos) <- observe flyer

  setFly flyer Throttle 0
  setFly flyer Yaw 63
  posRef <- newIORef (pos, 0)

  milliSleep waitTime
  (_, p') <- observe flyer
  milliSleep waitTime
  let initBrain = Brain intent (p', 0)
  setFly flyer Pitch 43
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

      lift $ putStrLn $ prettyPos pos'
      let vel = getVec (pos' - pos) |/| wt
      intent' <- lift $ enactIntent intent flyer pos' vel
      put $ Brain intent' (pos', pwr)

    else do
      iters <- lift $ fmap length $ 
        takeWhileIO not $
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

-- | The iteration time of the monkey (milliseconds)
waitTime :: Int
waitTime = 50

-- | Wait time in seconds.
wt :: Double
wt = (fromIntegral waitTime) / 1000.0
