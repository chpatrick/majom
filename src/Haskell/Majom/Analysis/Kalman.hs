-- | Module that implements a Kalman filter model.
module Majom.Analysis.Kalman (
  -- * Types
  Kalman,
  ) where

import Majom.Analysis.Model
import Majom.Common

data Kalman = 
  Kalman {
    kMap :: Acceleration -> Power
  }

-- Going to try to have the kalman model estimate where the "neutral point"
-- of the helicopter's thrust is (that is, where the acceleration is 0).
instance Model Kalman where
  createNewModel = undefined
  getMap h = undefined
  updateModel k (p,a) = undefined
  samples = undefined
