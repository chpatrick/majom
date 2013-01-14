-- | Defines the Flyable interface for the real helicopter, utilising
-- serial data transmission.

module Majom.Flyers.Helicopter (
  -- * Types
  Helicopter(..),
  -- * Functions
  startHelicopter
  ) where

import qualified Majom.Vision.Vision
import qualified Majom.Vision.Vision_Client as Client
import Majom.Vision.Vision_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Server

import Data.ByteString(pack)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import Network
import System.CPUTime
import System.Hardware.Serialport

import Majom.Flyers.Flyable

-- | A real helicopter!
data Helicopter = Helicopter { getCurrentOptions :: IORef OptionMap }

client = do 
  handle <- hOpen ("localhost", PortNumber 9090)
  let binProto = BinaryProtocol handle
  return (binProto, binProto)

-- | Starts an instance of the helicopter communication protocol.
startHelicopter :: IO Helicopter
startHelicopter = 
  Helicopter <$> (newIORef Map.empty)

instance Flyable Helicopter where
  setFly h o v = dropValM $ set h o v
  setFlyMany h vs = dropValM $ setMany h vs
  fly h = return ()
  observe h = do
    res <- Client.observe =<< client
    putStrLn $ show res
    let optVar = getCurrentOptions h
    pwr <- fmap (Map.! Throttle) $ readIORef optVar
    picoTime <- getCPUTime
    let pos = undefined --TODO Need to implement
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
  opts <- readIORef optsVar
  writeIORef optsVar $ process opts (o, clamp v)
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
