
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

addMenus :: Frame a -> IO ()
addMenus frame = do
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

gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]
    addMenus frame

    start <- button frame [text := "Start"]
    stop  <- button frame [text := "Stop"]
    pause <- button frame [text := "Pause"]
    resume <- button frame [text := "Resume"]
    
    tempo  <- hslider frame True 0 1000 [text := "Tempo"]
    gain   <- hslider frame True 0 1000 [text := "Gain"]
    volume <- hslider frame True 0 1000 [text := "Volume"]

    transport <- hgauge frame 1000 [text := "Volume", size := sz 750 30]
    
    
    let buttons = margin 10 $
            grid 10 10 [ [widget start, widget stop], 
                         [widget pause, widget resume] ]
        controls  = margin 10 $
            grid 10 0 [ [label "Tempo:", widget tempo], 
                        [label "Gain:", widget gain], 
                        [label "Volume:", widget volume] ]
        
    windowSetLayout frame $ margin 10 $ 
        column 0 [row 0 [buttons, shaped $ controls], 
                  shaped $ row 0 [margin 10 $ widget transport]]

    return ()

