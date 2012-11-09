-- | Module for simple image processing, including simple computer vision
-- tools.
module Majom.Vision.Simple (
  -- * Types
  Color,
  Size,
  Image,
  -- * Functions
  average,
  median,
  ) where

import Data.Array
import Data.List(sort)

-- | Basic RGB type.
type Color = (Int, Int, Int)

-- | Basic size type.
type Size = (Int, Int)

-- | Simple image type, covering the image size and its pixel values.
--   Assumes that all pixel values are present.
data Image = Image { imageSize :: Size, imageValues :: Array (Int, Int) Color }

-- | Reduces a three-tuple of lists into a single three-tuple.
reduce3 :: ([a] -> a) -> [(a, a, a)] -> (a, a, a)
reduce3 f = 
  (\(x, y, z) -> (f x, f y, f z)) . unzip3 

-- | Samples a 3x3 (or as close to possible) square around a point in
--   an array.
sample3x3 :: Array (Int,Int) a -> (Int, Int) -> Array (Int,Int) a
sample3x3 vals (i,j) = 
  array ((0,0),(2,2)) 
    [((x - (i-1), y - (j-1)), vals ! (x,y)) 
      |  x <- [i-1..i+1],
         y <- [j-1..j+1], 
         x >= 0, 
         y >= 0, 
         x <= (fst (snd (bounds vals))), 
         y <= (snd (snd (bounds vals)))]

-- | Performs elementary noise reduction by getting the local average.
average :: Image -> Image
average = filterImage sample3x3 (average' . elems)
  where
    average' :: [Color] -> Color
    average' = reduce3 (\x -> sum x `div` length x)

-- | Performs elementary noise reduction by getting the local median.
median :: Image -> Image
median = filterImage sample3x3 (median' . elems)
  where
    median' :: [Color] -> Color
    median' = reduce3 (\x -> sort x !! (length x `div` 2))

-- | Function type to sample points from an array around a given point.
type SampleMethod = Array (Int,Int) Color -> (Int, Int) -> Array (Int,Int) Color

-- | Function type to convolute (reduce) an array of colors 
--   into one single color.
type ConvolutionMethod = Array (Int,Int) Color -> Color

-- | Given a sampling function and convolution function, applys the combined
--   filter to the image to create a new image.
filterImage :: SampleMethod -> ConvolutionMethod -> Image -> Image
filterImage sample convolute (Image size@(sizeX, sizeY) vals) = 
  Image size newVals
  where
    newVals = 
      array (bounds vals) 
        [((i,j), operate (i,j)) | i <- [0..sizeX-1], j <- [0..sizeY-1]]
    operate = convolute . (sample vals)
