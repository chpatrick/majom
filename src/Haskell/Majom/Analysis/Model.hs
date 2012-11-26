-- | Module to perform statistical analysis on incoming data points and
-- come to an approximation map of Power -> Acceleration.
module Majom.Analysis.Model (
  -- * Types
  Model,
  -- * Classes
  -- * Functions
  getMap,
  createNewModel,
  updateModel
  ) where

import Majom.Common(Acceleration, Time, Position, (|*|), vector2)
import Numeric.LinearAlgebra

-- Input is a time and position
-- Output is an approximate mapping between power and acceleration
--
-- Start with an approximate map - say (0 -> -9.8), (70 -> 0), a straight line
-- Iteratively improve

data Model = Model { modelData :: [(Double, Double)], modelMap :: Double -> Double }
data Power = Int

getMap :: Model -> (Power -> Acceleration)
getMap model = undefined

createNewModel :: Model
createNewModel = (\x -> Model x (weightMap x)) [(0,0),(1,1)]

-- | Want a way to reuse the last model to save on computation...
updateModel :: Model -> (Double,Double) -> Model
updateModel (Model d m) input = Model d' (weightMap d')
  where 
    d' = (input : d)

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
