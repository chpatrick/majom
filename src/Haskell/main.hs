import System.Process
import qualified Graphics.GD as GD
import Majom.Graphics.Draw
import Majom.Vision.Simple

getDrawings = sequence [draw env (Object (125, y) sq) | y <- [0,25..250]]
  where
    env = Environment (250,250)
    sq  = getSquare 25 red

zero :: Int -> String -> String
zero n s = replicate (n - length s) '0' ++ s
 
testDrawings :: IO ()
testDrawings = do
  drawings <- getDrawings
  sequence_ $ map (\(file, img) -> save file img) $ zip ["img" ++ zero 3 (show i) ++ ".png" | i <- [1..]] drawings
  system "avconv -r 12 -b 16777216 -i img%03d.png out.mp4"
  system "rm *.png"
  return ()

testImageFilter :: IO ()
testImageFilter = do
  testImage <- GD.loadJpegFile "testsuite/lana.jpg"
  (sizeX, sizeY) <- GD.imageSize testImage
  colors <- sequence [GD.getPixel (i,j) testImage | i <- [0..sizeX-1], j <- [0..sizeY-1]]
  let pixels = [(i,j) | i <- [0..sizeX-1], j <- [0..sizeY-1]]
  outImage <- imageToGDImage . median $ createImage (sizeX, sizeY) $ zip pixels colors 
  GD.saveJpegFile 95 "out.jpg" outImage

main = do 
  putStrLn "Hello, majom!"
  testImageFilter
