-- | Defines the Flyable interface for the real helicopter, utilising
-- serial data transmission.

module Majom.Flyers.Helicopter (
  -- * Types
  Helicopter(..),
  -- * Functions
  startHelicopter
  ) where

import Data.ByteString(pack)
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network
import System.Hardware.Serialport

import Majom.Common
import Majom.Flyers.Flyable

import qualified Network.HTTP as HTTP
import Text.Regex.Posix

getPosition :: String -> (Double, Double, Double)
getPosition s
  | length m > 0 = read $ head m
  | otherwise = (0,0,0)
  where
    m = getAllTextMatches $ s =~ "[(].*[)]" :: [String]

getResponse :: IO String
getResponse = do
  r <- HTTP.simpleHTTP (HTTP.getRequest "http://localhost:8080/")
  return $ show r

get :: IO (Double, Double, Double)
get = return (1,2,3) --getPosition `fmap` getResponse
-- | A real helicopter!
data Helicopter = Helicopter { getCurrentOptions :: TVar OptionMap,
  getActive :: TVar Bool}

-- | Starts an instance of the helicopter communication protocol.
startHelicopter :: IO Helicopter
startHelicopter = do
  var <- (atomically $ newTVar Map.empty)
  s <- openSerial port defaultSerialSettings { 
        flowControl = Software }
  forkIO $ forever $ heliThread s var
  atomically $ Helicopter var <$> (newTVar False)

-- | Message passing time in milliseconds
stepTime :: Int
stepTime = 30

heliThread :: SerialPort -> TVar OptionMap -> IO ()
heliThread s var = do
  m <- atomically $ readTVar var
  sequence_ $ map foo $ Map.assocs m
  threadDelay (stepTime * 1000)
  where
    foo (o, v) = flip send (pack [fromIntegral $ fromEnum o, fromIntegral v]) s

{-TODO-- | Monitors a stack of helicopter instructions, then merges and sends 
 --- them at predefined intervals. 
 --- heliThread :: TVar [(Option, Int)] -> IO ()
 --- heliThread s = do
 --    -- check if s has elems
 --    --   merge and send s
 --    -- wait X ms
 --    -- repeat
-} 

instance Flyable Helicopter where
  setFly h o v = dropValM $ set h o v
  setFlyMany h vs = dropValM $ setMany h vs
  fly h = return ()
  observe h = do
    (x,y,z) <- get
    let optVar = getCurrentOptions h
    pwr <- fmap (Map.! Throttle) $ atomically $ readTVar optVar
    let pos = vector [x,y,z]
    --putStrLn $ show pos
    return (pwr, Position pos undefined)
  isActive h = atomically $ readTVar (getActive h)
  setActive h b = do 
    putStrLn "Foo!"
    atomically $ writeTVar (getActive h) b

-- | Drops monadic values we don't care about.
dropValM :: (Monad m) => m a -> m ()
dropValM x = x >> (return ())

-- | The default port for the arduino.
port :: String
port = "/dev/ttyACM0"

             
-- | Clamps the value to the feasible range
clamp :: Int -> Int 
clamp i 
  | i > 127   = 127
  | i < 0     = 0
  | otherwise = i

-- | Sets the option to the supplied value. Will fail if 
-- the serial port could not be set, or other IO related stuff. 
set :: Helicopter -> Option -> Int -> IO ()
set h o v = do
  let optsVar = getCurrentOptions h
  opts <- atomically $ readTVar optsVar
  atomically $ writeTVar optsVar $ process opts (o, clamp v)
  where
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert

type OptionMap = Map.Map Option Int

-- | Sends many options at once, in list order (later commands in
-- the list will be sent after (possibly overriding) previous commands).
setMany :: Helicopter -> [(Option, Int)] -> IO ()
setMany h = sequence_ . map (uncurry $ set h)
