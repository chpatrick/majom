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
data Brain = Brain { --brainIntent :: LandIntent,
                     --brainIntent :: HoverIntent,
                     brainIntent :: MultiIntent,
                     brainLast :: (Position, Power) }

desiredPos :: Vector
desiredPos = vector [0, 0.2, -1.7]

-- | Starts the monkey (starts te flyer too)
runMonkey :: (Flyable a) => a -> IO Brain
runMonkey flyer = do
  forkIO $ fly flyer
  --putStrLn "I am a monkey"

  runMonkey' flyer

-- | Runs the monkey (internal function).
runMonkey' :: (Flyable a) => a -> IO Brain
runMonkey' flyer = do
  milliSleep 5000
  surfaces <- lookAround flyer
  --putStrLn $ show (filter sSpecial surfaces)
  let hIntent = hoverAt $ Position desiredPos undefined
  let lIntent = landOn $ Position (vector [-0.1, -0.51, -1.65]) undefined
  --let intent = keepDoing $ withTiming (1000, waitTime)  $ doAll <&> hIntent <&> lIntent <&> hIntent
  {-
  let 
    intent = doAll 
      <&> hIntent
      <&> (if (length $ filter sSpecial surfaces) > 0
        then (landOn $ Position (sPoint $ head $ filter sSpecial surfaces) undefined)
        else lIntent)
  -}
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

  surfaces <- lift $ fmap (filter (not . sSpecial)) $ lookAround flyer
  if active 
    then do
      (Brain intent (pos, pwr)) <- get

      --lift $ putStrLn $ prettyPos pos'
      let vel = getVec (pos' - pos) |/| wt
      intent' <- lift $ enactIntent intent flyer pos' vel
      put $ Brain intent' (pos', pwr)

      --if or $ map (onIntersection pos' vel) surfaces 
      --  then (lift $ putStrLn "uh oh") 
      --  else (lift $ putStrLn "we're fine")

    else do
      iters <- lift $ fmap length $ 
        takeWhileIO (not . id) $
          repeat (milliSleep waitTime >> observe flyer >> isActive flyer)
      (Brain intent (pos,  _)) <- get
      obs@(pwr, pos') <- lift $ observe flyer
      let vel' = (getVec (pos' - pos)) |/| (wt * (fromIntegral iters))
      put $ Brain intent (pos',pwr)

  lift $ milliSleep waitTime

onIntersection :: Position -> Velocity -> Surface -> Bool
onIntersection p v s 
  | vectorSize v == 0 = False
  | vecDot l n == 0   = False
  | d < 0             = False
  | d <= 1            = True
  | otherwise         = False
  where
    l   = vectorUnit v
    l0  = getVec p
    p0  = sPoint s
    n   = sNorm s
    d   = (vecDot (p0 - l0) n) / (vecDot l n)

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
