-- | LeastSquares instantiation of the Model. Not really useful for
-- step by step learning, better for observing the making a one 
-- time judgement. Useful maybe for testing?
module Majom.Analysis.LeastSquares (
  -- * Classes
  -- * Types
  LeastSquares,
  -- * Functions
  setLeastSquares,
  ) where

import Majom.Analysis.Model
import Majom.Common
import qualified Numeric.LinearAlgebra as LA

-- | LeastSquares map type.
data LeastSquares = LeastSquares { lsMap :: Double -> Double }

instance Model LeastSquares where
  createNewModel = (\x -> LeastSquares (weightMap x)) [(0,0),(1,1)]
  getMap = undefined
  updateModel m _ = m
     
-- | Sets the least squares map using a one time analysis of 
-- a bunch of values.
setLeastSquares :: [(Power,Acceleration)] -> LeastSquares
setLeastSquares vals = 
  LeastSquares (weightMap d)
  where
    (pwrs,accels) = unzip vals
    -- | Only want y value of accel
    d = zip (map fromIntegral pwrs) (map vectorY accels)

-- | Constructs a weight map from inputs and observations.
weightMap :: [(Double, Double)] -> Double -> Double
weightMap d x = 
  head $ head $ LA.toLists $ (prep x) LA.<> (LA.reshape 1 $ leastSquares d)
  where
   prep :: Double -> LA.Matrix Double
   prep x = LA.fromColumns $ map ((LA.fromList [x])^) [0..2]

-- | Constructs a weight matrix from inputs and observations.
leastSquares :: [(Double, Double)] -> LA.Vector Double
leastSquares d = (LA.pinv $ (LA.ctrans x) LA.<> x) LA.<> (LA.ctrans x) LA.<> y_in
  where 
    (input, output) = unzip d
    x = LA.fromColumns $ map (x_in^) [0..2]
    x_in = LA.fromList input
    y_in = LA.fromList output
