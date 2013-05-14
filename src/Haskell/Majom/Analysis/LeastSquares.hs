-- | LeastSquares instantiation of the Model. Not really useful for
-- step by step learning, better for observing the making a one 
-- time judgement. Useful maybe for testing?

module Majom.Analysis.LeastSquares (
  -- * Classes
  -- * Types
  LeastSquares,
  -- * Functions
  ) where

import Majom.Analysis.Model
import Majom.Common
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Map as Map

-- | LeastSquares map type.
data LeastSquares = 
  LeastSquares { 
    lsMap :: Acceleration -> Power,
    lsSamples :: Map.Map Power Acceleration
    }

instance Model LeastSquares where
  createNewModel = (\x -> LeastSquares (constructMap x) $ Map.fromList x)
    [(80, vector [0,0,0]),(0,vector [0,-10,0])]
  getMap h = (\a -> if (vectorY a) <= 0 then 65 else 70) --lsMap
  updateModel ls@(LeastSquares m vs) (p,v) = ls
    -- | sane (p,v) $ Map.toList vs = updateMap $ LeastSquares m (Map.insert p v vs)
    -- | otherwise     = ls

-- | TODO Perform sanity checks - Just gravity check for now
sane :: (Power, Acceleration) -> [(Power, Acceleration)] -> Bool
sane (p,a) ps = 
  (vectorY a) >= (vectorY gravity)

-- | The number of samples to take on the initial assumption
-- before beginning map updates
sampleLimit :: Int
sampleLimit = 5

-- | Update the map from a given least squares model.
updateMap :: LeastSquares -> LeastSquares
updateMap ls@(LeastSquares _ vs) 
  | Map.size vs >= sampleLimit = LeastSquares (constructMap (Map.assocs vs)) vs
  | otherwise = ls

-- | Deconstructs the acceleration vector into three parts and performs
-- LS on each vector. 
constructMap :: [(Power, Acceleration)] -> (Acceleration -> Power)
constructMap vals =
  round . (weightMap $ zip [1.0,1.0..] $ zip ys $ map fromIntegral pwrs) . vectorY
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
-- [(Weight, (input, output))]
weightMap :: [(Double, (Double, Double))] -> Double -> Double
weightMap d x = 
  head $ head $ LA.toLists $ (prep x) LA.<> (LA.reshape 1 $ leastSquares d)
  where
   prep :: Double -> LA.Matrix Double
   prep x = LA.fromColumns $ map ((LA.fromList [x])^) [0..1]

-- | Constructs a ls matrix from inputs and observations, and
-- their corresponding weights..
leastSquares :: [(Double, (Double, Double))] -> LA.Vector Double
leastSquares xs = 
  (LA.pinv $ (LA.ctrans x) LA.<> w LA.<> x) LA.<> 
    (LA.ctrans x) LA.<> w LA.<> y_in
  where 
    (weights, d) = unzip xs
    w :: LA.Matrix Double
    w = listToDiag weights
    --w = LA.ident $ length d
    (input, output) = unzip d
    x = LA.fromColumns $ map (x_in^) [0..1]
    x_in = LA.fromList input
    y_in = LA.fromList output

-- | Converts a list of values to a diagonal matrix.
listToDiag :: [Double] -> LA.Matrix Double
listToDiag xs = LA.fromColumns 
  [LA.fromList $ (replicate i 0) ++
   [x] ++
   (replicate (length xs -(i+1)) 0) | (i, x) <- zip [0..] xs]
