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
                      brainLast :: (Position, Velocity, Power) }

runWatcher :: (Flyable a) => a -> IO Brain
runWatcher flyer = do
  let (f1, f2) = startDuoCopter flyer [Yaw,Pitch,Throttle,Correction]
  forkIO $ runGUI f1
  putStrLn "I am a watcher"
  
  (_, pos) <- observe flyer
  milliSleep waitTime
  (pwr, pos') <- observe flyer
  let initBrain = Brain createNewModel (pos', (getVec (pos' - pos)) |/| wt, pwr)
  handle <- openFile "out.csv" WriteMode
  execWatcherBrainT (forever $ watch flyer handle) initBrain

watch :: (Flyable a) => a -> Handle -> WatcherBrainT 
watch flyer output = do
  {-
  active <- lift $ isActive flyer
  if active then do
      (Brain model (pos, vel, pwr)) <- get
      obs@(pwr', pos') <- lift $ observe flyer
      let vel' = (getVec (pos' - pos)) |/| wt
      if (pwr' == pwr) then 
        do 
          let acc  = (vel' - vel) |/| wt
          let model' = updateModel model (pwr, acc)
          lift $ putStrLn $ (show pwr) ++ "," ++ (show (vectorY acc))
          put (Brain model' (pos', vel', pwr'))
        else 
          put (Brain model (pos', vel', pwr'))
      --lift $ putStrLn $ "New p est = " ++ (show (kMuHat model))
      --lift $ hPutStrLn output $ (show pwr) -- ++ "," ++ (show $ vectorY acc)
    
      lift $ milliSleep waitTime
    else do
      --lift $ putStrLn "Flyer is no longer active."
      (Brain model (pos, vel, pwr')) <- get
      obs@(pwr, pos') <- lift $ observe flyer
      put (Brain model (pos', (getVec (pos' - pos)) |/| wt, pwr'))
      lift $ hFlush output
      --lift $ hClose output
      lift $ milliSleep waitTime
      -}
  (_, pos) <- lift $ observe flyer
  lift $ putStrLn $ prettyPos pos
  lift $ hFlush stdout
  lift $ milliSleep waitTime

waitTime :: Int
waitTime = 50

wt :: Double
wt = (fromIntegral waitTime) / 1000.0
