-- | Module to perform the basic serial commands. Hides the actual serial port
-- dealings, and instead provides a basic command to set the parameters.

module Majom.Control.Comm (
  -- * Types
  Option,
  -- * Functions
  set
  ) where

import Data.ByteString(pack)
import System.Hardware.Serialport

-- | Basic enumerated type for different orders. Corresponds to the 
-- order types on the Arduino side.
data Option = Yaw | Pitch | Throttle | Correction
  deriving (Eq, Show, Enum)

-- | The default port for the arduino.
port :: String
port = "/dev/ttyACM0"

-- | Serial port to send data to.
s :: IO SerialPort
s = openSerial port defaultSerialSettings

-- | Sets the option to the supplied value. Will fail if 
-- the serial port could not be set, or other IO related stuff. 
-- Returns the number of bytes sent.
set :: Option -> Int -> IO Int
set o v = flip send (pack [fromIntegral $ fromEnum o, fromIntegral v]) =<< s

-- | Sends many options at once, in list order (later commands in
-- the list will be sent after (possibly overriding) previous commands).
-- Returns the total number of bytes sent.
setMany :: [(Option, Int)] -> IO Int
setMany = fmap sum . sequence . map (uncurry set)
