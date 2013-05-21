module Majom.Control.PID (
  PID(getKP, getKI, getKD),
  getMV,
  newPID,
  ) where

import Majom.Common

data PID = PID {
  lastErr :: Double, 
  sumErrs :: Double,
  getKP :: Double,
  getKI :: Double,
  getKD :: Double}
  deriving (Show)

getMV :: PID -> Double -> (PID, Double)
getMV pid err = (pid', out)
  where
    out = kp*err + ki*(errs) + kd*(err - (lastErr pid))
    errs = sumErrs pid
    pid' = pid { lastErr = err, sumErrs = (errs + err) }
    kp = getKP pid
    ki = getKI pid
    kd = getKD pid

newPID :: PID
newPID = PID 0 0 0 0 0
