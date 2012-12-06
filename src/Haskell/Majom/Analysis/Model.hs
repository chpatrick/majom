-- | Module to perform statistical analysis on incoming data points and
-- come to an approximation map of Power -> Acceleration.
module Majom.Analysis.Model (
  -- * Types
  -- * Classes
  Model(..),
  -- * Functions
  ) where

import Majom.Common(Acceleration, Power)

-- Input is a time and position
-- Output is an approximate mapping between power and acceleration
--
-- Start with an approximate map - say (0 -> -9.8), (70 -> 0), a straight line
-- Iteratively improve

-- | Default model functions.
class Model a where
  getMap :: a -> (Acceleration -> Power)
  updateModel :: a -> (Power, Acceleration) -> a
  createNewModel :: a
