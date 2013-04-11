-- | Module that implements a Kalman filter model.
module Majom.Analysis.Kalman (
  -- * Types
  Kalman(..),
  -- * Functions
  kalmanB,
  ) where

import Majom.Analysis.Model
import Majom.Common

import qualified Data.Map as M
import qualified Numeric.LinearAlgebra as LA

data Kalman = 
  Kalman {
    kMuHat :: Double,
    kP :: Double,
    kB :: Double
  }

-- Going to try to have the kalman model estimate where the "neutral point"
-- of the helicopter's thrust is (that is, where the acceleration is 0).
instance Model Kalman where
  createNewModel =
    Kalman 0.1 1 0
  getMap k = (\x -> round (((vectorY x) + 9.8)/(kMuHat k)))
  updateModel k (pwr,acc) =
    k { kMuHat = muHat, kP = p }
    where
      a = 1
      h = 1
      cov = 1
      sigma = 0.0513
      obs = ((vectorY acc) + 9.8)/(fromIntegral pwr)
      {-
      muHatEst = a*(kMuHat k)
      covEst = (a*cov*a) + sigma
      errorMu = obs - (h * muHatEst)
      errorCov = (h*cov*h) + (sigma*sigma)
      p = covEst*h*(1/errorCov)
      muHat = muHatEst + (p*errorMu)
      -}
      muHatEst = a*(kMuHat k)
      p' = a*(kP k)*a
      l = p'/(p' + (sigma*sigma))
      muHat = muHatEst + l*(obs - muHatEst)
      p = (1-l)*p'
  samples = undefined

kalmanB :: Kalman -> Power -> Kalman
kalmanB k b = k { kB = fromIntegral b }
