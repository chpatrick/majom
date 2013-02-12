module Main where
import Majom.Control.GUI
import Majom.Control.Monkey
import Majom.Flyers.VirtualHelicopter
import Majom.Flyers.Helicopter
import Majom.Flyers.Flyable

main = do
  putStrLn "Hello, majom!"
  --runGUI =<< startHelicopter
  --runGUI =<< spawnVirtualHelicopter
  --runMonkey =<< spawnVirtualHelicopter
  --runMonkeyWithHuman [Pitch] =<< spawnVirtualHelicopter
  runMonkeyWithHuman [Pitch] =<< startHelicopter
