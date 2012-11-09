module Majom.Vision.Simple (
  Color,
  Size,
  Image,
  average,
  median
  ) where

import Data.Array

type Color = (Int, Int, Int)
type Size = (Int, Int)
data Image = Image { imageSize :: Size, imageValues :: Array (Int, Int) Color }

-- | Performs elementary noise reduction by getting the local average.
average :: Image -> Image
average (Image size@(sizeX, sizeY) vals) = Image size newVals
  where
    newVals = array ((0,0),size) [((i,j), vals ! (i,j)) | i <- [1..sizeX-1], j <- [1..sizeY-1]]

-- | Performs elementary noise reduction by getting the local median.
median :: Image -> Image
median = undefined
