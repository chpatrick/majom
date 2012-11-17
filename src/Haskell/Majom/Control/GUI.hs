module Majom.Control.GUI (
  runGUI
  ) where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk( AttrOp ((:=)) )
import Majom.Control.Comm
import Data.IORef

runGUI :: IO ()
runGUI = do
  Gtk.initGUI
  window <- Gtk.windowNew
  button <- Gtk.buttonNew
  Gtk.set window [ Gtk.containerBorderWidth := 10, Gtk.containerChild := button ]
  Gtk.set button [ Gtk.buttonLabel := "Hello, World!" ]
  ref <- newIORef "Hi!"
  Gtk.onClicked button $ do
    val <- readIORef ref
    putStrLn val
    writeIORef ref (val ++ "!")
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.mainGUI
