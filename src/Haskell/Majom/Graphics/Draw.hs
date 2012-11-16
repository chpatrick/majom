-- | Module to draw simple shapes into an environment, for showing simulations.
module Majom.Graphics.Draw (
  -- * Types
  Environment(..),
  Object(..),
  -- * Functions
  getSphere,
  getSquare,
  draw,
  drawAndSave,
  save,
  red,
  green,
  blue
  ) where

import qualified Graphics.GD as GD

-- | Defines a drawing environment and its parameters.
data Environment = Environment { envGetSize :: (Int, Int) }

-- | Defines a drawing consisting of its points around an origin (0,0) (its anchor)
-- and the color of those points.
type Drawing = [(GD.Point, GD.Color)]

-- | Defines an object, composed of its position in space and its composite drawing.
data Object = Object { objectGetPosition :: (Int, Int), objectGetPixels :: Drawing } 

-- | Red color.
red :: GD.Color
red = GD.rgb 255 0 0

-- | Green color.
green :: GD.Color
green = GD.rgb 0 255 0

-- | Blue color.
blue :: GD.Color
blue = GD.rgb 0 0 255

-- | Gets a simple drawing of a sphere, anchor point in its centre.
getSphere :: Int -> GD.Color -> Drawing
getSphere = undefined

-- | Gets a simple drawing of a square, anchor point in its centre.
getSquare :: Int -> GD.Color -> Drawing
getSquare width color = [((x,y), color) | x <- [-half.. half], y <- [-half.. half]]
  where
    half = div width 2

-- | Draws an object into a white environment, given an environment and object.
draw :: Environment -> Object -> IO GD.Image
draw env obj = do
  img <- GD.newImage $ envGetSize env
  GD.fillImage (GD.rgb 255 255 255) img
  sequence_ $ map (\(p, c) -> GD.setPixel p c img) absDrawing
  return img
  where
    pos = objectGetPosition obj
    relDrawing = objectGetPixels obj
    absDrawing = map ((\(x, y) ((x',y'), color) -> ((x + x', y + y'), color)) pos) relDrawing

-- | Draws an object like in 'draw', but also saves it to the filesystem.
drawAndSave :: Environment -> Object -> String -> IO ()
drawAndSave env obj filename =
  GD.savePngFile filename =<< draw env obj

-- | Saves an image.
save :: String -> GD.Image -> IO ()
save = GD.savePngFile
