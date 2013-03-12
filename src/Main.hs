
module Main where

import Graphics.UI.WX

{-
    GUI:

        * Menus
            * File
                * Open...
                * Quit
            * Record
                * Start
                * Pause
                * Resume
                * Stop
            * Window
                * Minimize
                * Zoom


        * Window
            * Button: Prepare
            * Button: Start
            * Button: Pause

            * Slider: Position
            * Text: Section, Bar

-}

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]

    file            <- menuPane [text := "&File"]
    fileOpen        <- menuItem file [text := "&Open...\tCtrl+O"]
    fileQuit        <- menuItem file [text := "&Quit\tCtrl+Q"]
    
    record          <- menuPane [text := "&Record"]
    recordStart     <- menuItem record [text := "&Start\tCtrl+R"]
    recordPause     <- menuItem record [text := "&Pause\tCtrl+P"]
    recordResume    <- menuItem record [text := "&Resume\tCtrl+U"]
    recordStop      <- menuItem record [text := "&Stop\tCtrl+S"]

    window          <- menuPane [text := "&Window"]
    windowMinimize  <- menuItem window [text := "&Minimize\tCtrl+M"]
    windowZoom      <- menuItem window [text := "&Zoom"]

    set frame [
        menuBar            := [file, record, window],
        on (menu fileQuit) := close frame         
        ]

    start <- button frame [text := "Start"]
    stop  <- button frame [text := "Stop"]
    pause <- button frame [text := "Pause"]

    windowSetLayout frame $ margin 10 $ column 3 
        [widget start, widget stop, widget pause]

    return ()

