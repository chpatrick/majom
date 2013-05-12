{-# LANGUAGE TemplateHaskell #-}
module Majom.Control.MonocleMonkey (
  runMonocle,
  runMonocleWithHuman
  ) where

import Majom.Analysis.Kalman
import Majom.Common
import Majom.Control.GUI
import Majom.Flyers.Flyable
import Majom.Control.Monkey.Intent
import Majom.Control.Monkey.HoverIntent
import Majom.Flyers.Special.DuoCopter 

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Brain = Brain {
  _vModel :: Kalman,
  _hModel :: Kalman,
  _intent :: HoverIntent,
  _last   :: (Position, Velocity)
} 

makeLenses ''Brain

runMonocle = undefined
runMonocleWithHuman = undefined

monocleDo :: (Flyable a) => StateT (Brain, a) IO ()
monocleDo = undefined
  
