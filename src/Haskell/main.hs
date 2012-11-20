module Main where
import Majom.Control.GUI
import Majom.Flyers.VirtualHelicopter

main = do
  putStrLn "Hello, majom!"
  runGUI =<< spawnVirtualHelicopter
