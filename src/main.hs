import System.Process
import Majom.Graphics.Draw

getDrawings = sequence [draw env (Object (125, y) sq) | y <- [0,25..250]]
  where
    env = Environment (250,250)
    sq  = getSquare 25 red

zero :: Int -> String -> String
zero n s = replicate (n - length s) '0' ++ s

main = do 
  putStrLn "Hello, majom!"
  drawings <- getDrawings
  sequence_ $ map (\(file, img) -> save file img) $ zip ["img" ++ zero 3 (show i) ++ ".png" | i <- [1..]] drawings
  system "avconv -r 12 -b 16777216 -i img%03d.png out.mp4"
  system "rm *.png"
