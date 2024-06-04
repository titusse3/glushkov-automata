module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified GetExp as GE
import Data.Maybe (fromJust, isNothing)
import qualified NFA as N
import qualified Exp as E
import Data.GraphViz.Commands
    ( addExtension, runGraphviz, GraphvizOutput(Png) )

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    window <- new Gtk.Window [ #title := "Glushkov Automata" ]
    _ <- on window #destroy Gtk.mainQuit

    vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    hbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]

    entry <- new Gtk.Entry []
    button <- new Gtk.Button [ #label := "Envoyer" ]
    label <- new Gtk.Label [ #label := "Entrée une expression régulière" ]

    imageRef <- newIORef (Nothing :: Maybe Gtk.Image)

    let performAutomata = do
          text <- Gtk.entryGetText entry
          let result = GE.fromText text
          if isNothing result then do
            Gtk.labelSetText label "Erreur"
            currentImage <- readIORef imageRef
            case currentImage of
              Just img -> #remove vbox img
              Nothing -> return ()
          else do
            let validText = fromJust result
            Gtk.labelSetText label "Voici votre Automate"
            let automate = E.glushkov validText
            let automataDot = N.automatonToDotClustered automate
                              $ N.maximalOrbits automate
            let pngFile = "automate.png"
            _ <- addExtension (runGraphviz automataDot) Png "automate"
            -- Create an image widget from the PNG file
            newImage <- Gtk.imageNewFromFile pngFile
            Gtk.widgetSetSizeRequest newImage 150 150
            -- Replace the current image with the new one
            currentImage <- readIORef imageRef
            case currentImage of
              Just img -> #remove vbox img
              Nothing -> return ()
            writeIORef imageRef (Just newImage)
            -- Add the new image to the vertical box
            #packStart vbox newImage False False 5
            #showAll window

    _ <- on button #clicked performAutomata

    _ <- on entry #keyPressEvent $ \eventKey -> do
            keyval <- Gdk.getEventKeyKeyval eventKey
            -- Check if the Enter key was pressed
            if keyval == Gdk.KEY_Return then
                performAutomata >> return True
            else
                return False

    #packStart hbox entry True True 5
    #packStart hbox button False False 5
    #packStart vbox hbox False False 5
    #packStart vbox label False False 5

    -- Add the vertical box to the window
    #add window vbox

    -- Show all widgets in the window
    #showAll window
    Gtk.main
