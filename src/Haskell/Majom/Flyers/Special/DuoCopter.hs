{-# LANGUAGE ExistentialQuantification #-}
-- | Special flyer that lets two sources control one normal flyer at once, 
-- each having control of a particular part.
module Majom.Flyers.Special.DuoCopter (
  -- * Types
  DuoCopter(..),
  -- * Functions
  startDuoCopter
  ) where

import Majom.Flyers.Flyable

import Data.List

-- | There are two duocopters, each knowing its flyable and what options
-- it is allowed to control.
data DuoCopter = forall a. Flyable a =>
  DuoCopter { getAllowedOptions :: [Option], getFlyable :: a }

instance Flyable DuoCopter where
  setFly (DuoCopter os h) o v = do
    if o `elem` os then setFly h o v else return ()
  setFlyMany (DuoCopter os h) vs = 
    setFlyMany h $ filter ((`elem` os) . fst) vs
  fly (DuoCopter _ h) = fly h
  observe (DuoCopter _ h) = observe h

-- | Starts a duocopter instance, returning the corresponding 'mock copters'
-- that will be given to the two controls.
startDuoCopter :: (Flyable a) => a -> [Option] -> (DuoCopter, DuoCopter)
startDuoCopter h os = (DuoCopter f1 h, DuoCopter f2 h)
  where
    f1 = nub os
    f2 = [Yaw ..] \\ f1
