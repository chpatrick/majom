module Majom.Comm.Serial where
import Data.ByteString(pack)

data Command = Command { commandYaw :: Int, commandPitch :: Int, commandThrottle :: (Int,Int), commandCorrection :: Int }

setYaw :: Int -> IO ()
setYaw = undefined

setPitch :: Int -> IO ()
setPitch = undefined

setThrottle :: Int -> Int -> IO ()
setThrottle = undefined

setCorrection :: Int -> IO ()
setCorrection = undefined

sendCommand :: Command -> IO ()
sendCommand = undefined

--s <- openSerial port defaultSerialSettings
--let port = "/dev/ttyACM0"
--send s $ pack [instruction,value]
--make a command monad to carry around the port, commands and stuff?
