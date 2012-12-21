-- | LeastSquares instantiation of the Model. Not really useful for
-- step by step learning, better for observing the making a one 
-- time judgement. Useful maybe for testing?

-- Should try doing it in X segments - or take from a wide sampling
-- range? Keep sampling, removing if more 'varied' one appears or
-- if it gets too old - say after 10 seconds or so?
--
-- Hmmm... guess I'll do this tomorrow - maybe sketch it out and
-- see how it works. Store the structure and stuff in the data
-- type. 

module Majom.Analysis.LeastSquares (
  -- * Classes
  -- * Types
  LeastSquares,
  -- * Functions
  isSeeded,
  samples,
  setLeastSquares,
  ) where

import Majom.Analysis.Model
import Majom.Common
import qualified Numeric.LinearAlgebra as LA

-- | LeastSquares map type.
data LeastSquares = 
  LeastSquares { 
    lsMap :: Acceleration -> Power,
    lsSamples :: [(Power, Acceleration)]
    }

instance Model LeastSquares where
  createNewModel = (\x -> LeastSquares (constructMap x) [])
    [(50, vector [0,0]),(0,vector [0,-10])]
  getMap = lsMap
  updateModel (LeastSquares m vs) v = updateMap $ LeastSquares m (v:vs)
     
sampleLimit :: Int
sampleLimit = 10

samples = length . lsSamples

isSeeded :: LeastSquares -> Bool
isSeeded (LeastSquares _ vs) = length vs >= sampleLimit

updateMap :: LeastSquares -> LeastSquares
updateMap ls@(LeastSquares _ vs) 
  | length vs >= sampleLimit = LeastSquares (constructMap vs) vs
  | otherwise = ls

-- | Deconstructs the acceleration vector into three parts and performs
-- LS on each vector. 
constructMap :: [(Power, Acceleration)] -> (Acceleration -> Power)
constructMap vals =
  floor . (weightMap $ zip ys $ map fromIntegral pwrs) . vectorY
  where
    (pwrs, accels) = unzip vals
    ys = map vectorY accels

-- | Sets the least squares map using a one time analysis of 
-- a bunch of values.
setLeastSquares :: [(Power,Acceleration)] -> LeastSquares
setLeastSquares vals = 
  undefined --LeastSquares (weightMap d)
  where
    (pwrs,accels) = unzip vals
    -- | Only want y value of accel
    d = zip (map fromIntegral pwrs) (map vectorY accels)

-- Everything from here downwards deals with single dimension numbers

-- | Constructs a weight map from inputs and observations.
weightMap :: [(Double, Double)] -> Double -> Double
weightMap d x = 
  head $ head $ LA.toLists $ (prep x) LA.<> (LA.reshape 1 $ leastSquares d)
  where
   prep :: Double -> LA.Matrix Double
   prep x = LA.fromColumns $ map ((LA.fromList [x])^) [0..1]

-- | Constructs a weight matrix from inputs and observations.
leastSquares :: [(Double, Double)] -> LA.Vector Double
leastSquares d = (LA.pinv $ (LA.ctrans x) LA.<> x) LA.<> (LA.ctrans x) LA.<> y_in
  where 
    (input, output) = unzip d
    x = LA.fromColumns $ map (x_in^) [0..1]
    x_in = LA.fromList input
    y_in = LA.fromList output
