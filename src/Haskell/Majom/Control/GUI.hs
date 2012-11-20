-- | Provides a GUI to take user input for flying the helicopter.
-- d/f - Throttle
-- a/s - Correction
-- up/down - Pitch
-- left/right - Yaw
module Majom.Control.GUI (
  -- * Functions
  runGUI
  ) where

import Majom.Flyers.Flyable

import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk( AttrOp ((:=)) )

-- | Starts the GUI
runGUI :: (Flyable a) => a -> IO ()
runGUI flyer = do
  Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.set window [ Gtk.containerBorderWidth := 10 ]
  Gtk.onDestroy window Gtk.mainQuit
  
  vals <- fmap (Map.fromList . zip [Yaw, Pitch, Throttle, Correction])
        $ mapM newIORef [63, 63, 0, 63]

  Gtk.on window Gtk.keyPressEvent $ do
    interpretKeyPress flyer vals
  Gtk.widgetSetCanFocus window True
  Gtk.widgetShowAll window
  forkIO $ fly flyer
  Gtk.mainGUI


-- | Takes a key press and turns it into a helicopter command. Could use refactoring.
interpretKeyPress :: (Flyable a) => a -> Map.Map Option (IORef Int) -> Gtk.EventM Gtk.EKey Bool
interpretKeyPress flyer valMap = do
  keyName <- Gtk.eventKeyName
  case keyName of
    "Up" -> do -- Pitch forwards
      liftIO $ putStrLn "Up"
      value <- liftIO $ readIORef $ valMap Map.! Pitch
      liftIO $ setFly flyer Pitch (value - 10)
      liftIO $ writeIORef (valMap Map.! Pitch) (value - 10)
      return True
    "Down" -> do -- Pitch backwards
      liftIO $ putStrLn "Down"
      value <- liftIO $ readIORef $ valMap Map.! Pitch
      liftIO $ setFly flyer Pitch (value + 10)
      liftIO $ writeIORef (valMap Map.! Pitch) (value + 10)
      return True
    "Left" -> do -- Yaw left
      liftIO $ putStrLn "Left"
      value <- liftIO $ readIORef $ valMap Map.! Yaw
      liftIO $ setFly flyer Yaw (value + 10)
      liftIO $ writeIORef (valMap Map.! Yaw) (value + 10)
      return True
    "Right" -> do -- Yaw right
      liftIO $ putStrLn "Right"
      value <- liftIO $ readIORef $ valMap Map.! Yaw
      liftIO $ setFly flyer Yaw (value - 10)
      liftIO $ writeIORef (valMap Map.! Yaw) (value - 10)
      return True
    "d" -> do -- Throttle down
      liftIO $ putStrLn "d"
      value <- liftIO $ readIORef $ valMap Map.! Throttle
      liftIO $ setFly flyer Throttle (value - 10)
      liftIO $ writeIORef (valMap Map.! Throttle) (value - 10)
      return True
    "f" -> do -- Throttle up
      liftIO $ putStrLn "f"
      value <- liftIO $ readIORef $ valMap Map.! Throttle
      liftIO $ setFly flyer Throttle (value + 10)
      liftIO $ writeIORef (valMap Map.! Throttle) (value + 10)
      return True
    "a" -> do -- Correction left
      liftIO $ putStrLn "a"
      value <- liftIO $ readIORef $ valMap Map.! Correction
      liftIO $ setFly flyer Correction (value - 10)
      liftIO $ writeIORef (valMap Map.! Correction) (value - 10)
      return True
    "s" -> do -- Correction right
      liftIO $ putStrLn "s"
      value <- liftIO $ readIORef $ valMap Map.! Correction
      liftIO $ setFly flyer Correction (value + 10)
      liftIO $ writeIORef (valMap Map.! Correction) (value + 10)
      return True
    "x" -> do -- Reset
      liftIO $ putStrLn "x"
      liftIO $ setFlyMany flyer [(Pitch, 63), (Throttle, 0), (Yaw, 63), (Correction, 63)]
      liftIO $ writeIORef (valMap Map.! Pitch) 63
      liftIO $ writeIORef (valMap Map.! Yaw) 63
      liftIO $ writeIORef (valMap Map.! Correction) 63
      liftIO $ writeIORef (valMap Map.! Throttle) 0
      return True
    "Escape" -> do -- Quit the program
      liftIO Gtk.mainQuit
      return True
    otherwise -> return False
