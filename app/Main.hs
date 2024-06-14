module Main (main) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified GetExp as GE
import Data.Maybe (isJust, fromJust, isNothing)
import qualified NFA as N
import qualified NFAG as NG
import qualified Exp as E
import Data.GraphViz.Commands (addExtension, runGraphviz, GraphvizOutput(Svg))
import System.Process (callCommand)
import System.Directory (doesFileExist)
import qualified JsonToNFA as JNFA
import qualified Data.Text as T
import Text.Read
import qualified Data.Set as Set
import Data.Foldable

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    window <- new Gtk.Window [#title := "Glushkov Automata"]
    _ <- on window #destroy Gtk.mainQuit

    Gtk.windowSetPosition window Gtk.WindowPositionCenter

    Gtk.windowSetIconFromFile window "logo.png"
    #setDefaultSize window 1080 720
    Gtk.windowSetResizable window False

    -- Load CSS
    provider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath provider "style.css"
    screen <- Gdk.screenGetDefault
    case screen of
        Just scr -> Gtk.styleContextAddProviderForScreen scr provider 
                    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
        Nothing -> putStrLn "Error: Could not get default screen."

    mainVBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    contentVBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    scrolledWindow <- new Gtk.ScrolledWindow []
    #setPolicy scrolledWindow Gtk.PolicyTypeAutomatic Gtk.PolicyTypeAutomatic

    inputBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    optionsBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    entry <- new Gtk.Entry []
    button <- new Gtk.Button [#label := "Envoyer"]
    importButton <- new Gtk.Button [#label := "Importer"]
    label <- new Gtk.Label [#label := "Entrée une expression régulière"]
    choiceButton <- new Gtk.Button [#label := "Options"]
    comboBox <- new Gtk.ComboBoxText []

    imageRef <- newIORef (Nothing :: Maybe Gtk.Image)
    showOptions <- newIORef False

    automataRef <- newIORef (Nothing :: Maybe (NG.NFAG Int Char))
    orbitRef <- newIORef (Nothing :: Maybe (Set.Set Int))

    -- Initialize the combo box with "Non-clustered" as the default option
    Gtk.comboBoxTextAppendText comboBox "Non-clustered"
    Gtk.comboBoxTextAppendText comboBox "Clustered"
    Gtk.comboBoxSetActive comboBox 0 -- Set "Non-clustered" as the active item

    -- Initially hide the options box by not packing it into inputBox
    #packStart optionsBox comboBox False False 5

    -- Create radio buttons for orbit selection
    radioButtonManual <- new Gtk.RadioButton []
    #setLabel radioButtonManual "Orbit manuel"
    radioButtonMaximal <- new Gtk.RadioButton [#group := radioButtonManual]
    #setLabel radioButtonMaximal "Orbit maximal"

    -- Create input and dropdown for orbit selection
    manualInput <- new Gtk.Entry []
    manualApplyButton <- new Gtk.Button [#label := "Appliquer"] 
    maximalDropdown <- new Gtk.ComboBoxText []
    dropdownApplyButton <- new Gtk.Button [#label := "Appliquer"]

    -- Create frame for the image
    imageFrame <- new Gtk.Frame [#label := "Image"]
    imageInFrame <- new Gtk.Image []
    Gtk.containerAdd imageFrame imageInFrame

    -- Increase the size of the image frame
    Gtk.widgetSetSizeRequest imageFrame 500 (-1)

    -- Create properties grid
    propertiesGrid <- new Gtk.Grid []
    #setHexpand propertiesGrid True
    #setVexpand propertiesGrid True
    #setBorderWidth propertiesGrid 10

    -- properties table
    let properties = ["Orbit", "Stable", "Transverse", "Strongly Stable", 
                      "Strongly Transverse", "In", "Out"]

    propLabels <- mapM (\prop -> new Gtk.Label [#label := prop]) properties
    valueLabels <- mapM (\_ -> new Gtk.Label [#label := "Unknown"]) properties
    mapM_ (\(i, (propLabel, valueLabel)) -> do
             Gtk.widgetSetName valueLabel "unknown"
             Gtk.widgetSetName propLabel "custom-label"
             Gtk.widgetSetName valueLabel "custom-label"
             Gtk.gridAttach propertiesGrid propLabel 0 (fromIntegral i) 1 1
             Gtk.gridAttach propertiesGrid valueLabel 1 (fromIntegral i) 1 1
             propStyleContext <- Gtk.widgetGetStyleContext propLabel
             valueStyleContext <- Gtk.widgetGetStyleContext valueLabel
             Gtk.styleContextAddClass propStyleContext "custom-label"
             Gtk.styleContextAddClass valueStyleContext "custom-label"
          ) (zip ([0..] :: [Int]) (zip propLabels valueLabels))

    -- Box to contain image frame and properties grid
    infoBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #packStart infoBox imageFrame True True 5
    #packStart infoBox propertiesGrid False False 5

    manualInputMessage <- new Gtk.Label [#label := ""]

    -- Box to contain orbit selection input or dropdown
    orbitBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    manualInputBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    dropdownBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

    #packStart manualInputBox manualInput True True 5
    #packStart manualInputBox manualApplyButton False False 5
    #packStart dropdownBox maximalDropdown True True 5
    #packStart dropdownBox dropdownApplyButton False False 5

    #packStart orbitBox manualInputBox False False 5
    #packStart orbitBox dropdownBox False False 5
    #packStart orbitBox manualInputMessage False False 5
    #packStart orbitBox infoBox False False 5
    #setVisible manualInputBox False
    #setVisible dropdownBox False

    -- Center the radio buttons
    #setHalign radioButtonManual Gtk.AlignCenter
    #setHalign radioButtonMaximal Gtk.AlignCenter

    -- Box to contain radio buttons and orbit selection
    radioButtonsBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #packStart radioButtonsBox radioButtonManual True True 5
    #packStart radioButtonsBox radioButtonMaximal True True 5

    radioBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #packStart radioBox radioButtonsBox False False 5
    #packStart radioBox orbitBox False False 5

    -- Box to conditionally display radio buttons
    radioBoxContainer <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    let updateRadioBoxVisibility = do
          currentAutomata <- readIORef automataRef
          if isJust currentAutomata
            then do
              #packStart radioBoxContainer radioBox False False 5
            else do
              Gtk.containerForeach radioBoxContainer $ \child -> do
                Gtk.containerRemove radioBoxContainer child
          #showAll window

    let updateOrbitSelection = do
          activeButton <- Gtk.toggleButtonGetActive radioButtonManual
          if activeButton
            then do
              #setVisible manualInputBox True
              #setVisible dropdownBox False
            else do
              #setVisible manualInputBox False
              #setVisible dropdownBox True

    let updateImage svgFile = do
          newImage <- Gtk.imageNewFromFile svgFile
          Gtk.widgetSetSizeRequest newImage 500 500
          currentImage <- readIORef imageRef
          forM_ currentImage (#remove contentVBox)
          writeIORef imageRef (Just newImage)
          #packStart contentVBox newImage False False 5
          updateRadioBoxVisibility
          updateOrbitSelection

    let updateMaximalOrbitsDropdown nfa = do
          Gtk.comboBoxTextRemoveAll maximalDropdown
          let orbits = N.maximalOrbits nfa
          mapM_ (\(n, x) -> Gtk.comboBoxTextAppendText maximalDropdown $ 
                            T.pack (show n) <> ". " <> N.orbitToText x <> ". " <> N.orbitToText x) 
                $ zip ([1 .. ] :: [Int]) orbits

    let performAutomata = do
          text <- Gtk.entryGetText entry
          let result = GE.expFromText text
          if isNothing result then do
            Gtk.labelSetText label "Erreur"
            currentImage <- readIORef imageRef
            forM_ currentImage (#remove contentVBox)
          else do
            let validText = fromJust result :: E.Exp Char
            Gtk.labelSetText label "Voici votre Automate"
            activeText <- Gtk.comboBoxTextGetActiveText comboBox
            let clustered = case activeText of
                              Just "Clustered" -> True
                              _ -> False
            let automate = E.glushkov validText
            writeIORef automataRef (Just automate)
            updateMaximalOrbitsDropdown automate
            let automataDot = if clustered
                              then N.automatonToDotClustered 
                                (N.maximalOrbits automate) automate 
                              else N.automatonToDot automate
            let svgFile = "automate.svg"
            _ <- addExtension (runGraphviz automataDot) Svg "automate"
            callCommand $ "rsvg-convert -w 600 " ++ svgFile ++ " -o " ++ svgFile
            updateImage svgFile

    let importAutomata = do
          dialog <- Gtk.new Gtk.FileChooserDialog 
            [#title := "Importer Automate"
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
                  :: IO (Either String (NG.NFAG Int Char))
                case parsed of
                  Left err -> Gtk.labelSetText label $ T.pack err
                  Right nfa -> do
                    writeIORef automataRef (Just nfa)
                    updateMaximalOrbitsDropdown nfa
                    Gtk.labelSetText label "Automate importé"
                    activeText <- Gtk.comboBoxTextGetActiveText comboBox
                    let clustered = case activeText of
                                      Just "Clustered" -> True
                                      _ -> False
                    let automataDot = if clustered
                                      then N.automatonToDotClustered 
                                        (N.maximalOrbits nfa) nfa
                                      else N.automatonToDot nfa
                    let svgFile = "imported_automate.svg"
                    _ <- addExtension (runGraphviz automataDot) Svg 
                        "imported_automate"
                    callCommand $ "rsvg-convert -w 700 " ++ svgFile ++ " -o " 
                                  ++ svgFile
                    updateImage svgFile

    let onOrbitChange selectedOrbit = do
            maybeNfa <- readIORef automataRef
            case maybeNfa of
              Just nfa -> do
                let propertiesValues = [
                      N.isOrbit selectedOrbit nfa,
                      N.isStableOrbit selectedOrbit nfa,
                      N.isTransversOrbit selectedOrbit nfa,
                      N.isStronglyStableOrbit selectedOrbit nfa,
                      N.isStronglyTransversOrbit selectedOrbit nfa]
                mapM_ (\(i, value) -> do
                  let valueText = if value then "Vrai" else "Faux" :: T.Text
                  _ <- #setLabel (valueLabels !! i) valueText
                  let valueClass = if value then "true" else "false"
                  Gtk.widgetSetName (valueLabels !! i) valueClass
                  valueStyleContext <- Gtk.widgetGetStyleContext 
                        (valueLabels !! i)
                  Gtk.styleContextRemoveClass valueStyleContext "true"
                  Gtk.styleContextRemoveClass valueStyleContext "false"
                  Gtk.styleContextAddClass valueStyleContext valueClass
                  ) (zip [0..] propertiesValues)
                #setLabel (valueLabels !! 5) $ 
                  N.orbitToText $ N.orbitIn selectedOrbit nfa
                #setLabel (valueLabels !! 6) $ 
                  N.orbitToText $ N.orbitOut selectedOrbit nfa
                let a' = N.extractListStateAutomata selectedOrbit nfa
                case a' of 
                  Just automata' -> do
                    let automataDot = N.automatonToDot automata'
                    let svgFile = "orbit.svg"
                    _ <- addExtension (runGraphviz automataDot) Svg "orbit"
                    callCommand $ "rsvg-convert -w 500 " ++ svgFile ++ " -o " 
                      ++ svgFile

                    -- Mettre l'image dans le cadre destiné
                    Gtk.imageClear imageInFrame
                    Gtk.imageSetFromFile imageInFrame (Just svgFile)
                    -- Gtk.widgetSetSizeRequest imageInFrame 300 300
                  Nothing -> return ()
              Nothing -> return ()

    let onManualInputChanged = do
            text <- Gtk.entryGetText manualInput
            let parsed = textToSet text
            case parsed of
              Just orbit -> do
                Gtk.labelSetText manualInputMessage $ 
                                 "Orbit possiblement valide : " 
                                 <> N.orbitToText orbit 
                writeIORef orbitRef (Just orbit)
                onOrbitChange orbit
              _ -> do
                Gtk.labelSetText manualInputMessage "Erreur : Orbit invalide."
                writeIORef orbitRef Nothing

    let onMaximalDropdownChanged = do
            activeText <- Gtk.comboBoxTextGetActiveText maximalDropdown
            case activeText of
              Just text -> do
                let parsed = textToSet $ T.drop 1 $ snd $ T.breakOn "." text
                case parsed of
                  Just orbit -> do
                    Gtk.labelSetText manualInputMessage $ 
                                    "Orbit valide : " <> N.orbitToText orbit 
                    writeIORef orbitRef (Just orbit)
                    onOrbitChange orbit
                  _ -> do
                    Gtk.labelSetText manualInputMessage 
                      "Erreur : Orbit invalide."
                    writeIORef orbitRef Nothing
              _ -> return ()

    _ <- on button #clicked performAutomata
    _ <- on importButton #clicked importAutomata
    _ <- on radioButtonManual #toggled updateOrbitSelection
    _ <- on radioButtonMaximal #toggled updateOrbitSelection
    _ <- on manualApplyButton #clicked onManualInputChanged
    _ <- on dropdownApplyButton #clicked onMaximalDropdownChanged

    _ <- on manualInput #keyPressEvent $ \eventKey -> do
            keyval <- Gdk.getEventKeyKeyval eventKey
            if keyval == Gdk.KEY_Return then
                onManualInputChanged >> return True
            else
                return False

    _ <- on maximalDropdown #keyPressEvent $ \eventKey -> do
            keyval <- Gdk.getEventKeyKeyval eventKey
            if keyval == Gdk.KEY_Return then
                onMaximalDropdownChanged >> return True
            else
                return False

    _ <- on entry #keyPressEvent $ \eventKey -> do
            keyval <- Gdk.getEventKeyKeyval eventKey
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
    #packStart contentVBox inputBox False False 5
    #packStart contentVBox label False False 5
    #packStart mainVBox contentVBox False False 5

    -- Add the main vertical box to the scrolled window
    #add scrolledWindow mainVBox

    -- Add the radio buttons container to the main vertical box
    #packEnd mainVBox radioBoxContainer False False 5

    -- Add the scrolled window to the main window
    #add window scrolledWindow

    -- Show all widgets in the window
    #showAll window

    -- Update visibility based on the initial state of automataRef
    updateRadioBoxVisibility

    Gtk.main

textToSet :: T.Text -> Maybe (Set.Set Int)
textToSet text = do
    let trimmed = T.strip text
    let withoutBraces = T.filter (\x -> x /= '{' && x /= '}') trimmed
    let elements = T.splitOn "," withoutBraces
    parsedElements <- mapM (readMaybe . T.unpack . T.strip) elements
    return $ Set.fromList parsedElements
