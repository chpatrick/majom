module Majom.Analysis.LeastSquares (
  -- * Classes
  -- * Types
  LeastSquares,
  -- * Functions
  setLeastSquares,
  ) where

import Majom.Analysis.Model
import Majom.Common(Power, Acceleration)
import Numeric.LinearAlgebra

data LeastSquares = LeastSquares { lsData :: [(Double, Double)], lsMap :: Double -> Double }

instance Model LeastSquares where
  createNewModel = (\x -> LeastSquares x (weightMap x)) [(0,0),(1,1)]
  getMap = undefined
  updateModel (LeastSquares d m) (pwr,accel) = LeastSquares d' (weightMap d')
    where 
      -- Only want the y value for now
      input = (fromIntegral pwr, snd accel)
      d' = (input : d)
     
setLeastSquares :: [(Power,Acceleration)] -> LeastSquares
setLeastSquares vals = 
  LeastSquares d (weightMap d)
  where
    (pwrs,accels) = unzip vals
    d = zip (map fromIntegral pwrs) (map snd accels)

weightMap :: [(Double, Double)] -> Double -> Double
weightMap d x = 
  head $ head $ toLists $ (prep x) <> (reshape 1 $ leastSquares d)
  where
   prep :: Double -> Matrix Double
   prep x = fromColumns $ map ((fromList [x])^) [0..2]

leastSquares :: [(Double, Double)] -> Vector Double
leastSquares d = (pinv $ (ctrans x) <> x) <> (ctrans x) <> y_in
  where 
    (input, output) = unzip d
    x = fromColumns $ map (x_in^) [0..2]
    x_in = fromList input
    y_in = fromList output
