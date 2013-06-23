-- | Module for PID control.
module Majom.Control.PID (
  PID(getKP, getKI, getKD),
  getMV,
  newPID,
  ) where

import Majom.Common

-- | Data structure for holding various PID parameters
data PID = PID {
  lastErr :: Double, 
  sumErrs :: Double,
  getKP :: Double,
  getKI :: Double,
  getKD :: Double}
  deriving (Show)

-- | Get the next input given an error
getMV :: PID -> Double -> (PID, Double)
getMV pid err = (pid', out)
  where
    out = kp*err + ki*(errs) + kd*(err - (lastErr pid))
    errs = sumErrs pid
    pid' = pid { lastErr = err, sumErrs = (errs + err) }
    kp = getKP pid
    ki = getKI pid
    kd = getKD pid

-- | Creates a new blank PID controller
newPID :: PID
newPID = PID 0 0 0 0 0
