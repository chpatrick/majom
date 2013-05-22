-- | Provides a GUI to take user input for flying the helicopter.
-- d/f - Throttle
-- a/s - Correction
-- up/down - Pitch
-- left/right - Yaw
module Majom.Control.GUI (
  -- * Functions
  runGUI,
  ) where

import Majom.Common
import Majom.Flyers.Flyable
import Majom.Control.PID

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk( AttrOp ((:=)) )

-- | Starts the GUI
runGUI :: (Flyable a) => a -> IO ()
runGUI flyer = do
  Gtk.initGUI
  guiSetup flyer 
  forkIO $ fly flyer
  Gtk.mainGUI

guiSetup :: (Flyable a) => a -> IO ()
guiSetup flyer = do
  window <- Gtk.windowNew
  Gtk.set window [ Gtk.containerBorderWidth := 10 ]
  Gtk.onDestroy window Gtk.mainQuit
  
  vals <- fmap (Map.fromList . zip [Yaw, Pitch, Throttle, Correction])
        $ mapM newIORef [63, 63, 0, 63]

  vb <- Gtk.vBoxNew False 0
  Gtk.containerAdd window vb
   
  hb1 <- Gtk.hBoxNew False 0
  Gtk.boxPackStart vb hb1 Gtk.PackNatural 0
  hb2 <- Gtk.hBoxNew False 0
  Gtk.boxPackStart vb hb2 Gtk.PackNatural 0
            
  lab1 <- Gtk.labelNew (Just "Enter KP")
  Gtk.boxPackStart hb1 lab1 Gtk.PackNatural 0
  nrfield1 <- Gtk.entryNew
  Gtk.entrySetText nrfield1 "5"
  Gtk.boxPackStart hb1 nrfield1 Gtk.PackNatural 5

  lab1 <- Gtk.labelNew (Just "Enter KI")
  Gtk.boxPackStart hb1 lab1 Gtk.PackNatural 0
  nrfield2 <- Gtk.entryNew
  Gtk.entrySetText nrfield2 "0.1"
  Gtk.boxPackStart hb1 nrfield2 Gtk.PackNatural 5

  lab1 <- Gtk.labelNew (Just "Enter KD")
  Gtk.boxPackStart hb1 lab1 Gtk.PackNatural 0
  nrfield3 <- Gtk.entryNew
  Gtk.entrySetText nrfield3 "5"
  Gtk.boxPackStart hb1 nrfield3 Gtk.PackNatural 5

  Gtk.onEntryActivate nrfield1 (updateKP flyer nrfield1)
  Gtk.onEntryActivate nrfield2 (updateKI flyer nrfield2)
  Gtk.onEntryActivate nrfield3 (updateKD flyer nrfield3)

  accbox    <- Gtk.hBoxNew False 0
  Gtk.boxPackStart vb accbox Gtk.PackNatural 5
  im <- Gtk.imageNewFromStock Gtk.stockApply Gtk.IconSizeButton
  acceptButton <- Gtk.buttonNewWithLabel "Accept"
  Gtk.buttonSetImage acceptButton im
  Gtk.boxPackStart accbox acceptButton Gtk.PackRepel 0

  Gtk.onPressed acceptButton $ do
    updateKP flyer nrfield1
    updateKI flyer nrfield2
    updateKD flyer nrfield3

  Gtk.on window Gtk.keyPressEvent $ do
    interpretKeyPress flyer vals 
  Gtk.widgetSetCanFocus window True
  Gtk.widgetShowAll window

updateKP :: (Flyable a) => a -> Gtk.Entry -> IO ()
updateKP flyer fld = do 
  val <- fmap read $ Gtk.entryGetText fld
  pid <- getController flyer
  putStrLn $ "Setting KP to " ++ (show val)
  setController flyer (pid { getKP = val })

updateKI :: (Flyable a) => a -> Gtk.Entry -> IO ()
updateKI flyer fld = do 
  val <- fmap read $ Gtk.entryGetText fld
  pid <- getController flyer
  putStrLn $ "Setting KI to " ++ (show val)
  setController flyer (pid { getKI = val })

updateKD :: (Flyable a) => a -> Gtk.Entry -> IO ()
updateKD flyer fld = do 
  val <- fmap read $ Gtk.entryGetText fld
  pid <- getController flyer
  putStrLn $ "Setting KD to " ++ (show val)
  setController flyer (pid { getKD = val })

-- | Takes a key press and turns it into a helicopter command.
interpretKeyPress :: (Flyable a) => a -> Map.Map Option (IORef Int) -> Gtk.EventM Gtk.EKey Bool
interpretKeyPress flyer valMap = do
  keyName <- Gtk.eventKeyName
  case keyName of
    "Up" -> do -- Pitch forwards
      liftIO $ chValue "Up" Pitch (-10)
      return True
    "Down" -> do -- Pitch backwards
      liftIO $ chValue "Down" Pitch (10)
      return True
    "Left" -> do -- Yaw left
      liftIO $ chValue "Left" Yaw (10)
      return True
    "Right" -> do -- Yaw right
      liftIO $ chValue "Right" Yaw (-10)
      return True
    "g" -> do -- Throttle down
      liftIO $ chValue "g" Throttle (-1)
      return True
    "h" -> do -- Throttle up
      liftIO $ chValue "h" Throttle (1)
      return True
    "d" -> do -- Throttle down
      liftIO $ chValue "d" Throttle (-10)
      return True
    "f" -> do -- Throttle up
      liftIO $ chValue "f" Throttle (10)
      return True
    "a" -> do -- Correction left
      liftIO $ chValue "a" Correction (-10)
      return True
    "s" -> do -- Correction right
      liftIO $ chValue "s" Correction (10)
      return True
    "x" -> do -- Reset
      liftIO $ putStrLn "x"
      liftIO $ setFlyMany flyer [(Pitch, 63), (Throttle, 0), (Yaw, 63), (Correction, 63)]
      liftIO $ writeIORef (valMap Map.! Pitch) 63
      liftIO $ writeIORef (valMap Map.! Yaw) 63
      liftIO $ writeIORef (valMap Map.! Correction) 63
      liftIO $ writeIORef (valMap Map.! Throttle) 0
      return True
    "o" -> do -- Observe
      liftIO $ putStrLn "o"
      (_,pos) <- liftIO $ observe flyer
      pwr <- liftIO $ readIORef $ valMap Map.! Throttle
      liftIO $ putStrLn $ show (pwr, pos)
      return True
    "q" -> do -- Start observation
      active <- liftIO $ isActive flyer
      liftIO $ setActive flyer (not active)
      return True
    "Escape" -> do -- Quit the program
      liftIO $ setActive flyer False
      liftIO $ putStrLn "Escape"
      liftIO Gtk.mainQuit
      return True
    otherwise -> return False
    where
      chValue :: String -> Option -> Int -> IO ()
      chValue key o v = do
        --putStrLn key
        putStrLn $ "Setting " ++ (show o) ++ " to " ++ (show v)
        value <- readIORef $ valMap Map.! o
        setFly flyer o (value + v)
        writeIORef (valMap Map.! o) (value + v)

