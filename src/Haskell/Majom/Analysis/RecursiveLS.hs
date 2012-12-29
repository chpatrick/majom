-- | Recursive least squares implementation.
module Majom.Analysis.RecursiveLS (
  -- * Classes
  -- * Types
  -- * Functions
  ) where

import Majom.Analysis.Model
import Majom.Common
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra as LA

data RLS = RLS { rlsW :: LA.Vector Double, rlsP :: LA.Matrix Double }
  deriving (Show, Eq)

rlsInit :: RLS
rlsInit = RLS (LA.fromList [0]) (LA.scale 10000 $ LA.ident 1)

rlsUpdate :: LA.Vector Double -> Double -> RLS -> RLS
rlsUpdate u d (RLS w p) =
  RLS w' (LA.scaleRecip forget (p - p'))
  where
    pi = LA.vXm u p
    forget = 0.5
    gamma = forget + (pi LA.<.> u)
    k = LA.scaleRecip gamma pi
    alpha = d - (w LA.<.> u)
    w' = w + (LA.scale alpha k)
    p' = (LA.fromColumns [k]) LA.<> (LA.fromRows [pi])

rls1DInit :: (Double, Double)
rls1DInit = (0.0, 1000000)

rls1D :: Double -> Double -> (Double, Double) -> (Double, Double)
rls1D u d (w,p) = 
  (w', (p - p')/forget) 
  where
    pi = u * p
    gamma = forget + (pi * u)
    forget = 0.5
    k = pi / gamma
    alpha = d - (w * u)
    w' = w + (k * alpha)
    p' = k * pi

