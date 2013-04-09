-- | Controller that just observes a human flying the helicopter, using
-- different analysis methods to judge their effectiveness.
module Majom.Control.Watcher (
  -- * Functions
  runWatcher,
  ) where

import Majom.Analysis.Model
import Majom.Analysis.Kalman
import Majom.Common
import Majom.Control.GUI
import Majom.Flyers.Flyable
import Majom.Flyers.Special.DuoCopter

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State

import System.IO

type WatcherBrainT = StateT Brain IO ()

execWatcherBrainT :: WatcherBrainT -> Brain -> IO Brain
execWatcherBrainT mk k = execStateT mk k

data Brain = Brain  { brainModel :: Kalman,
                      brainLast :: (Position, Velocity) }

runWatcher :: (Flyable a) => a -> IO Brain
runWatcher flyer = do
  let (f1, f2) = startDuoCopter flyer [Yaw,Pitch,Throttle,Correction]
  forkIO $ runGUI f1
  putStrLn "I am a watcher"
  
  (_, pos) <- observe flyer
  let initBrain = Brain createNewModel (pos, undefined)
  handle <- openFile "out.csv" WriteMode
  execWatcherBrainT (forever $ watch flyer handle) initBrain

watch :: (Flyable a) => a -> Handle -> WatcherBrainT 
watch flyer output = do
  (Brain model (pos, _)) <- get
  obs@(pwr, pos') <- lift $ observe flyer

  lift $ hPutStrLn output $ prettyVec (pos' - pos)
  lift $ putStrLn $ "Current velocity is " ++ (prettyVec (pos' - pos))
  lift $ milliSleep waitTime

waitTime :: Int
waitTime = 50

milliSleep :: Int -> IO ()
milliSleep = threadDelay . (*) 1000
