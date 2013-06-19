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
import Majom.Control.PID
import Majom.Flyers.Flyable

import qualified Network.HTTP as HTTP
import Text.Regex.Posix

getPosition :: String -> (Double, Double, Double, Double, Int)
getPosition s
  | length m > 0 = read $ head m
  | otherwise = (0,0,0,0,0)
  where
    m = getAllTextMatches $ s =~ "[(].*[)]" :: [String]

getResponse :: IO String
getResponse = do
  r <- HTTP.simpleHTTP (HTTP.getRequest "http://localhost:8080/")
  return $ show r

get :: IO (Double, Double, Double, Double, Int)
get = getPosition `fmap` getResponse

-- | A real helicopter!
data Helicopter = Helicopter { getCurrentOptions :: TVar OptionMap,
  getActive :: TVar Bool, getPID :: TVar PID, getSurfaces :: TVar [Surface]}

getSurfs' :: String -> [Surface]
getSurfs' s
  | length m > 0 = map surfacify vs
  | otherwise = []
  where
    surfacify (a,b) = Surface (vector b) (vectorUnit $ vector a) False
    vs :: [([Double], [Double])]
    vs = read $ head m
    m = getAllTextMatches $ s =~ "[[].*[]]" :: [String]

getSurfacesResponse :: IO String
getSurfacesResponse = do
  r <- HTTP.simpleHTTP (HTTP.getRequest "http://localhost:8081/")
  return $ show r

getSurfs :: IO [Surface]
getSurfs = getSurfs' `fmap` getSurfacesResponse

-- | Starts an instance of the helicopter communication protocol.
startHelicopter :: IO Helicopter
startHelicopter = do
  --surfaces <- getSurfs
  let surfaces = []
  --putStrLn $ show surfaces
  
  var <- (atomically $ newTVar Map.empty)
  s <- openSerial port defaultSerialSettings { 
        flowControl = Software }
  forkIO $ forever $ heliThread s var

  atomically $ Helicopter var <$> (newTVar False) <*> (newTVar (newPID)) <*> (newTVar surfaces)

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

instance Flyable Helicopter where
  setFly h o v = dropValM $ set h o v
  setFlyMany h vs = dropValM $ setMany h vs
  fly h = return ()
  observe h = do
    putStrLn "OBSERVE"
    (x,y,z,o,v) <- get
    let optVar = getCurrentOptions h
    pwr <- fmap (Map.! Throttle) $ atomically $ readTVar optVar
    case v of
      0 -> setActive h True
      1 -> setActive h False
    let pos = vector [x,y,z]
    return (pwr, Position pos o)
  isActive h = atomically $ readTVar (getActive h)
  setActive h b = atomically $ writeTVar (getActive h) b
  getController h = atomically $ readTVar (getPID h)
  setController h p = atomically $ writeTVar (getPID h) p
  lookAround h = atomically $ readTVar (getSurfaces h)

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
