-- | Defines the Flyable interface for the real helicopter, utilising
-- serial data transmission.

module Majom.Flyers.Helicopter (
  -- * Types
  Helicopter(..),
  -- * Functions
  startHelicopter
  ) where

import Data.ByteString(pack)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network
import System.CPUTime
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
get = getPosition `fmap` getResponse
-- | A real helicopter!
data Helicopter = Helicopter { getCurrentOptions :: TVar OptionMap }

-- | Starts an instance of the helicopter communication protocol.
startHelicopter :: IO Helicopter
startHelicopter = do
  var <- (atomically $ newTVar Map.empty)
  forkIO $ forever $ heliThread var
  return $ Helicopter var

-- | Message passing time in milliseconds
stepTime :: Int
stepTime = 1000

heliThread :: TVar OptionMap -> IO ()
heliThread var = do
  putStrLn "ROFL"
  threadDelay (stepTime * 1000)

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
    picoTime <- getCPUTime
    let pos = vector [x,y,z]
    putStrLn $ show pos
    return (pwr, pos, (fromInteger picoTime) / (fromInteger cpuTimePrecision))

-- | Drops monadic values we don't care about.
dropValM :: (Monad m) => m a -> m ()
dropValM x = x >> (return ())

-- | The default port for the arduino.
port :: String
port = "/dev/ttyACM0"

-- | Serial port to send data to.
s :: IO SerialPort
s = openSerial port defaultSerialSettings { 
      flowControl = Software }
             
-- | Clamps the value to the feasible range
clamp :: Int -> Int 
clamp i 
  | i > 127   = 127
  | i < 0     = 0
  | otherwise = i

-- | Sets the option to the supplied value. Will fail if 
-- the serial port could not be set, or other IO related stuff. 
-- Returns the number of bytes sent.
set :: Helicopter -> Option -> Int -> IO Int
set h o v = do
  let optsVar = getCurrentOptions h
  opts <- atomically $ readTVar optsVar
  atomically $ writeTVar optsVar $ process opts (o, clamp v)
  foo `seq` foo
  where
    foo = flip send (pack [fromIntegral $ fromEnum o, fromIntegral v]) =<< s
    process :: OptionMap -> (Option, Int) -> OptionMap
    process = flip $ uncurry Map.insert

type OptionMap = Map.Map Option Int

-- | Sends many options at once, in list order (later commands in
-- the list will be sent after (possibly overriding) previous commands).
-- Returns the total number of bytes sent.
setMany :: Helicopter -> [(Option, Int)] -> IO Int
setMany h = fmap sum . sequence . map (uncurry $ set h)
