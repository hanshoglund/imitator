
module Main where


import Graphics.UI.Gtk

{-
    GUI:
    
        * Window
            * Button: Prepare
            * Button: Start
            * Button: Pause
            
            * Slider: Position
            * Text: Section, Bar

-}
                         

main :: IO ()
main = do
    initGUI
    window <- windowNew

    hpaned <- hBoxNew False 10
    set window [ containerBorderWidth := 10, containerChild := hpaned ]
    
    do
        buttons <- vButtonBoxNew
        set hpaned [ containerBorderWidth := 10, containerChild := buttons ]

        -- slider <- hScaleNewWithRange 0 1 0.01
        -- set hpaned [ containerBorderWidth := 10, containerChild := slider ]

        volume <- scaleButtonNew IconSizeButton 0 100 2 ["A", "B", "C"]
        scaleButtonSetIcons volume ["STOCK_HELP"]
        set hpaned [ containerBorderWidth := 10, containerChild := volume ]

        progress <- progressBarNew
        set hpaned [ containerBorderWidth := 10, containerChild := progress ]


        do  prepare <- buttonNew
            set buttons [ containerChild := prepare ]
            set prepare [ buttonLabel := "Prepare" ]
            onClicked prepare  $ do
                putStrLn "Clicked Prepare"
                set progress [progressBarFraction := 0]
                
            start <- buttonNew
            set buttons [ containerChild := start ]
            set start [ buttonLabel := "Start" ]
            onClicked start $ do
                putStrLn "Clicked Start"
                set progress [progressBarFraction := 0.5]
    
            stop <- buttonNew
            set buttons [ containerChild := stop ]
            set stop [ buttonLabel := "Stop" ]
            onClicked stop $ do
                putStrLn "Clicked Stop"
                set progress [progressBarFraction := 1.0]

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

