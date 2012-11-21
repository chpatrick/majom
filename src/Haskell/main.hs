module Main where
import Majom.Control.GUI
import Majom.Flyers.VirtualHelicopter
import Majom.Flyers.Helicopter

main = do
  putStrLn "Hello, majom!"
  runGUI =<< spawnVirtualHelicopter
