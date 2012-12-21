module Main where
import Majom.Control.GUI
import Majom.Control.Monkey
import Majom.Flyers.VirtualHelicopter
import Majom.Flyers.Helicopter

main = do
  putStrLn "Hello, majom!"
  --runGUI =<< startHelicopter
  --runGUI =<< spawnVirtualHelicopter
  runMonkey =<< spawnVirtualHelicopter
