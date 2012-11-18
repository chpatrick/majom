module Majom.Control.GUI (
  runGUI
  ) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk( AttrOp ((:=)) )
import Majom.Control.Comm
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.IO.Class
runGUI :: IO ()
runGUI = do
  Gtk.initGUI
  window <- Gtk.windowNew
  buttonBox <- Gtk.vButtonBoxNew
  Gtk.set window [ Gtk.containerBorderWidth := 10, Gtk.containerChild := buttonBox ]
  Gtk.onDestroy window Gtk.mainQuit
  
  btnThrottleUp <- Gtk.buttonNew
  btnThrottleDown <- Gtk.buttonNew
  btnYawUp <- Gtk.buttonNew
  btnYawDown <- Gtk.buttonNew
  btnPitchUp <- Gtk.buttonNew
  btnPitchDown <- Gtk.buttonNew
  btnCorrectionUp <- Gtk.buttonNew
  btnCorrectionDown <- Gtk.buttonNew

  Gtk.set btnThrottleUp [ Gtk.buttonLabel := "Throttle +" ]
  Gtk.set btnThrottleDown [ Gtk.buttonLabel := "Throttle -" ]
  Gtk.set btnYawUp [ Gtk.buttonLabel := "Yaw +" ]
  Gtk.set btnYawDown [ Gtk.buttonLabel := "Yaw -" ]
  Gtk.set btnPitchUp [ Gtk.buttonLabel := "Pitch +" ]
  Gtk.set btnPitchDown [ Gtk.buttonLabel := "Pitch -" ]
  Gtk.set btnCorrectionUp [ Gtk.buttonLabel := "Correction +" ]
  Gtk.set btnCorrectionDown [ Gtk.buttonLabel := "Correction -" ]

  Gtk.set buttonBox [ Gtk.containerChild := button
                    | button <-
                      [btnThrottleUp, btnThrottleDown,
                       btnYawUp,btnYawDown,
                       btnPitchUp, btnPitchDown,
                       btnCorrectionUp, btnCorrectionDown ]]
                       
  Gtk.set buttonBox [Gtk.buttonBoxLayoutStyle := Gtk.ButtonboxCenter]

  ref <- newIORef 0

  vals <- fmap (Map.fromList . zip [Yaw, Pitch, Throttle, Correction])
        $ mapM newIORef [63, 63, 0, 63]

  Gtk.onClicked btnThrottleUp $ do
    val <- readIORef ref
    set Throttle val
    writeIORef ref (val + 10)

  Gtk.widgetAddEvents window [Gtk.KeyPressMask]
  Gtk.on window Gtk.keyPressEvent $ do
    interpretKeyPress vals
  Gtk.widgetSetCanFocus window True
  Gtk.widgetShowAll window
  Gtk.mainGUI

interpretKeyPress :: Map.Map Option (IORef Int) -> Gtk.EventM Gtk.EKey Bool
interpretKeyPress valMap = do
  keyName <- Gtk.eventKeyName
  case keyName of
    "Up" -> do -- Pitch forwards
      liftIO $ putStrLn "Up"
      value <- liftIO $ readIORef $ valMap Map.! Pitch
      liftIO $ set Pitch (value - 10)
      liftIO $ writeIORef (valMap Map.! Pitch) (value - 10)
      return True
    "Down" -> do -- Pitch backwards
      liftIO $ putStrLn "Down"
      value <- liftIO $ readIORef $ valMap Map.! Pitch
      liftIO $ set Pitch (value + 10)
      liftIO $ writeIORef (valMap Map.! Pitch) (value + 10)
      return True
    "Left" -> do -- Yaw left
      liftIO $ putStrLn "Left"
      value <- liftIO $ readIORef $ valMap Map.! Yaw
      liftIO $ set Yaw (value + 10)
      liftIO $ writeIORef (valMap Map.! Yaw) (value + 10)
      return True
    "Right" -> do -- Yaw right
      liftIO $ putStrLn "Right"
      value <- liftIO $ readIORef $ valMap Map.! Yaw
      liftIO $ set Yaw (value - 10)
      liftIO $ writeIORef (valMap Map.! Yaw) (value - 10)
      return True
    "d" -> do -- Throttle down
      liftIO $ putStrLn "d"
      value <- liftIO $ readIORef $ valMap Map.! Throttle
      liftIO $ set Throttle (value - 10)
      liftIO $ writeIORef (valMap Map.! Throttle) (value - 10)
      return True
    "f" -> do -- Throttle up
      liftIO $ putStrLn "f"
      value <- liftIO $ readIORef $ valMap Map.! Throttle
      liftIO $ set Throttle (value + 10)
      liftIO $ writeIORef (valMap Map.! Throttle) (value + 10)
      return True
    "a" -> do -- Correction left
      liftIO $ putStrLn "a"
      value <- liftIO $ readIORef $ valMap Map.! Correction
      liftIO $ set Correction (value - 10)
      liftIO $ writeIORef (valMap Map.! Correction) (value - 10)
      return True
    "s" -> do -- Correction right
      liftIO $ putStrLn "s"
      value <- liftIO $ readIORef $ valMap Map.! Correction
      liftIO $ set Correction (value + 10)
      liftIO $ writeIORef (valMap Map.! Correction) (value + 10)
      return True
    "x" -> do -- Reset
      liftIO $ putStrLn "x"
      liftIO $ setMany [(Pitch, 63), (Throttle, 0), (Yaw, 63), (Correction, 63)]
      liftIO $ writeIORef (valMap Map.! Pitch) 63
      liftIO $ writeIORef (valMap Map.! Yaw) 63
      liftIO $ writeIORef (valMap Map.! Correction) 63
      liftIO $ writeIORef (valMap Map.! Throttle) 0
      return True
    "Escape" -> do -- Quit the program
      liftIO Gtk.mainQuit
      return True
    otherwise -> return False
