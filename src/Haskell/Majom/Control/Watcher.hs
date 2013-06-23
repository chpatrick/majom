-- | Controller that just observes a human flying the helicopter, using
-- different analysis methods to judge their effectiveness.
module Majom.Control.Watcher (
  -- * Functions
  runWatcher,
  ) where

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

data Brain = Brain  { brainLast :: Position }

-- | Runs the watcher control method
runWatcher :: (Flyable a) => a -> IO Brain
runWatcher flyer = do
  let (f1, f2) = startDuoCopter flyer [Yaw,Pitch,Throttle,Correction]
  forkIO $ runGUI f1
  putStrLn "I am a watcher"
  getChar 
  (_, pos) <- observe flyer
  let initBrain = Brain pos
  putStrLn "Go!"
  execWatcherBrainT (forever $ watch flyer) initBrain

-- | The main loop for watching the helicopter
watch :: (Flyable a) => a -> WatcherBrainT 
watch flyer = do
  (Brain pos) <- get
  obs@(_, pos') <- lift $ observe flyer
  surfaces <- lift $ fmap (filter (not . sSpecial)) $ lookAround flyer
  let vel = getVec (pos' - pos) |/| wt

  lift $ putStrLn $ show $ map (snd.(onIntersection pos' vel)) surfaces
  if or $ map (fst.(onIntersection pos' vel)) surfaces 
    then (lift $ setFly flyer Pitch 93) 
    else (lift $ putStrLn "Phew!")

  put (Brain pos')
  lift $ putStrLn $ prettyPos pos'
  lift $ milliSleep waitTime

-- | The wait time between iterations
waitTime :: Int
waitTime = 50

-- | Works out if the helicopter will intersect with a surface
onIntersection :: Position -> Velocity -> Surface -> (Bool,Double)
onIntersection p v s 
  | vectorSize v' == 0 = (False,0)
  | vecDot l n == 0    = (False,0)
  | d < 0              = (False,d)
  | d <= 0.5             = (True,d)
  | otherwise          = (False,d)
  where
    v'  = vector [vectorX v, 0, vectorZ v]
    l   = vectorUnit v'
    l0  = getVec p
    p0  = sPoint s
    n   = sNorm s
    d   = (vecDot (p0 - l0) n) / (vecDot l n)

-- | Converts the waittime from ms to secs
wt :: Double
wt = (fromIntegral waitTime) / 1000.0
