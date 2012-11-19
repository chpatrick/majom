-- | Defines the Flyable interface for the real helicopter, utilising
-- serial data transmission.

module Majom.Flyers.Helicopter (
  -- * Types
  Helicopter(..),
  ) where

import Data.ByteString(pack)
import System.Hardware.Serialport
import Majom.Control.Flyable

-- | A real helicopter!
data Helicopter = Helicopter

instance Flyable Helicopter where
  setFly _ o v = dropValM $ set o v
  setFlyMany _ vs = dropValM $ setMany vs

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
              
-- | Sets the option to the supplied value. Will fail if 
-- the serial port could not be set, or other IO related stuff. 
-- Returns the number of bytes sent.
set :: Option -> Int -> IO Int
set o v = foo `seq` foo
  where
    foo = flip send (pack [fromIntegral $ fromEnum o, fromIntegral v]) =<< s

-- | Sends many options at once, in list order (later commands in
-- the list will be sent after (possibly overriding) previous commands).
-- Returns the total number of bytes sent.
setMany :: [(Option, Int)] -> IO Int
setMany = fmap sum . sequence . map (uncurry set)
