module Main where
import Majom.Control.GUI
import Majom.Control.Monkey
import Majom.Control.Watcher
import Majom.Flyers.VirtualHelicopter
import Majom.Flyers.Helicopter
import Majom.Flyers.Flyable
import System.IO


main = do
  hSetBuffering stdout NoBuffering
  --putStrLn "Hello, majom!"
  --runGUI =<< startHelicopter
  --runGUI =<< spawnVirtualHelicopter
  --runMonkey =<< spawnVirtualHelicopter
  --runMonkeyWithHuman [Pitch, Yaw, Correction] =<< spawnVirtualHelicopter
  --runMonkeyWithHuman [Pitch, Yaw, Correction] =<< startHelicopter
  --runWatcher =<< spawnVirtualHelicopter
  runWatcher =<< startHelicopter
