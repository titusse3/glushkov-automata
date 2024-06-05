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
    ( addExtension, runGraphviz, GraphvizOutput(Svg) )
import System.Process (callCommand)
import System.Directory (doesFileExist)
import qualified JsonToNFA as JNFA
import qualified Data.Text as T

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    window <- new Gtk.Window [ #title := "Glushkov Automata" ]
    _ <- on window #destroy Gtk.mainQuit

    Gtk.windowSetIconFromFile window "logo.png"
    #setDefaultSize window 800 600

    vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    inputBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    hbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
    optionsBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]

    entry <- new Gtk.Entry []
    button <- new Gtk.Button [ #label := "Envoyer" ]
    importButton <- new Gtk.Button [ #label := "Importer" ]
    label <- new Gtk.Label [ #label := "Entrée une expression régulière" ]
    choiceButton <- new Gtk.Button [ #label := "Options" ]
    comboBox <- new Gtk.ComboBoxText []

    imageRef <- newIORef (Nothing :: Maybe Gtk.Image)
    showOptions <- newIORef False

    -- Initialize the combo box with "Non-clustered" as the default option
    Gtk.comboBoxTextAppendText comboBox "Non-clustered"
    Gtk.comboBoxTextAppendText comboBox "Clustered"
    Gtk.comboBoxSetActive comboBox 0 -- Set "Non-clustered" as the active item

    -- Initially hide the options box by not packing it into inputBox
    #packStart optionsBox comboBox False False 5

    let updateImage svgFile = do
          newImage <- Gtk.imageNewFromFile svgFile
          Gtk.widgetSetSizeRequest newImage 500 500
          currentImage <- readIORef imageRef
          case currentImage of
            Just img -> #remove vbox img
            Nothing -> return ()
          writeIORef imageRef (Just newImage)
          #packStart vbox newImage False False 5
          #showAll window

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
            activeText <- Gtk.comboBoxTextGetActiveText comboBox
            let clustered = case activeText of
                              Just "Clustered" -> True
                              _ -> False
            let automate = E.glushkov validText
            let automataDot = if clustered
                              then N.automatonToDotClustered automate 
                                   (N.maximalOrbits automate)
                              else N.automatonToDot automate
            let svgFile = "automate.svg"
            _ <- addExtension (runGraphviz automataDot) Svg "automate"
            callCommand $ "rsvg-convert -w 700 " ++ svgFile ++ " -o " ++ svgFile
            updateImage svgFile

    let importAutomata = do
          dialog <- Gtk.new Gtk.FileChooserDialog 
            [ #title := "Importer Automate"
            , #action := Gtk.FileChooserActionOpen
            , #transientFor := window
            , #modal := True
            ]
          _ <- Gtk.dialogAddButton dialog "Cancel" 
               (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
          _ <- Gtk.dialogAddButton dialog "Open" 
               (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
          _ <- Gtk.dialogRun dialog
          filename <- Gtk.fileChooserGetFilename dialog
          Gtk.widgetDestroy dialog
          case filename of
            Nothing -> return ()
            Just path -> do
              exists <- doesFileExist path
              if not exists then
                Gtk.labelSetText label "Fichier non trouvé"
              else do
                parsed <- JNFA.parseNFA path 
                  :: IO (Either String (N.NFA Int T.Text))
                case parsed of
                  Left err -> Gtk.labelSetText label $ T.pack err
                  Right nfa -> do
                    Gtk.labelSetText label "Automate importé"
                    activeText <- Gtk.comboBoxTextGetActiveText comboBox
                    let clustered = case activeText of
                                      Just "Clustered" -> True
                                      _ -> False
                    let automataDot = if clustered
                                      then N.automatonToDotClustered nfa 
                                           (N.maximalOrbits nfa)
                                      else N.automatonToDot nfa
                    let svgFile = "imported_automate.svg"
                    _ <- addExtension (runGraphviz automataDot) Svg 
                         "imported_automate"
                    callCommand $ "rsvg-convert -w 700 " ++ svgFile ++ " -o " 
                                  ++ svgFile
                    updateImage svgFile

    _ <- on button #clicked performAutomata
    _ <- on importButton #clicked importAutomata

    _ <- on entry #keyPressEvent $ \eventKey -> do
            keyval <- Gdk.getEventKeyKeyval eventKey
            -- Check if the Enter key was pressed
            if keyval == Gdk.KEY_Return then
                performAutomata >> return True
            else
                return False

    _ <- on window #keyPressEvent $ \eventKey -> do
            state <- Gdk.getEventKeyState eventKey
            keyval <- Gdk.getEventKeyKeyval eventKey
            let ctrlPressed = Gdk.ModifierTypeControlMask `elem` state
            if ctrlPressed && keyval == Gdk.KEY_o then
                importAutomata >> return True
            else
                return False

    _ <- on choiceButton #clicked $ do
            currentShowOptions <- readIORef showOptions
            if currentShowOptions then do
                #remove inputBox optionsBox
                writeIORef showOptions False
            else do
                #packStart inputBox optionsBox False False 5
                writeIORef showOptions True
            #showAll window

    #packStart hbox entry True True 5
    #packStart hbox button False False 5
    #packStart hbox importButton False False 5
    #packStart hbox choiceButton False False 5
    #packStart inputBox hbox False False 5
    #packStart vbox inputBox False False 5
    #packStart vbox label False False 5

    -- Add the vertical box to the window
    #add window vbox

    -- Show all widgets in the window
    #showAll window
    Gtk.main
