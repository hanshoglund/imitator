
-- {-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Control.Applicative

import Control.Concurrent (forkIO, forkOS, threadDelay)
import System.Exit

import Graphics.UI.WX hiding (Event, Reactive)

import Music.Imitator.Reactive
import Music.Imitator.Reactive.Chan


addMenus :: Frame a -> IO (String -> Event Int, String -> Sink ())
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
        on (menu fileQuit) := close frame,
        on (menu recordStart) := return ()
        ]

    let sources    = error "No such source"
    let sinks = error "No such sink"
    return (sources, sinks)



addWidgets :: Frame a -> IO (String -> Event Int, String -> Sink Int)
addWidgets frame = do
    
    -- Create widgets
    start       <- button frame [text := "Start"]
    stop        <- button frame [text := "Stop"]
    pause       <- button frame [text := "Pause"]
    resume      <- button frame [text := "Resume"]

    tempo       <- hslider frame True 0 1000 [text := "Tempo"]
    gain        <- hslider frame True 0 1000 [text := "Gain"]
    volume      <- hslider frame True 0 1000 [text := "Volume"]

    transport   <- hgauge frame 1000 [text := "Volume", size := sz 750 30]

    -- Set layout
    let buttons = margin 10 $ boxed "Transport" $
            grid 10 10 [ [widget start, widget pause],
                         [widget stop, widget resume] ]

        controls  = margin 10 $ boxed "Controls" $
            grid 10 5 [ [label "Tempo:", widget tempo],
                        [label "Gain:", widget gain],
                        [label "Volume:", widget volume] ]

        status = margin 10 $ boxed "Status" $
            column 0 [
                label "CPU (%):",
                label "Memory (MB):",
                label "Server:",
                label "Server mean CPU (%):",
                label "Server peak CPU (%):"
            ]

        positioning = shaped $ margin 10 $ column 10 [
            widget transport,
            row 10 [label "Time:", label "Section:", label "Bar:"]
            ]

    windowSetLayout frame $ margin 10 $
        column 0 [row 0 [buttons, shaped $ controls, status],
                  positioning]

    -- Create sources/sinks
    (startA, startE)            <- newSource
    (stopA, stopE)              <- newSource
    (pauseA, pauseE)            <- newSource
    (resumeA, resumeE)          <- newSource
    (tempoA, tempoE)            <- newSource
    (gainA, gainE)              <- newSource
    (volumeA, volumeE)          <- newSource

    (tempoB, tempoS)            <- newSink
    (gainB, gainS)              <- newSink
    (volumeB, volumeS)          <- newSink
    (transportB, transportS)    <- newSink

    set start   [on command := startA 0]
    set stop    [on command := stopA 0]
    set pause   [on command := pauseA 0]
    set resume  [on command := resumeA 0]

    set tempo   [on command := get tempo  selection >>= tempoA]
    set gain    [on command := get gain   selection >>= gainA]
    set volume  [on command := get volume selection >>= volumeA]

    let refreshWidgets = do
        tempoB      >>= set' tempo selection
        gainB       >>= set' gain selection
        volumeB     >>= set' volume selection
        transportB  >>= set' transport selection
        return ()

    timer frame [interval := 100, on command := refreshWidgets]

    let sources     = \x -> case x of
        { "start"         -> startE
        ; "stop"          -> stopE
        ; "pause"         -> pauseE
        ; "resume"        -> resumeE
        ; "tempo"         -> tempoE
        ; "gain"          -> gainE
        ; "volume"        -> volumeE
        ;  _              -> error "No such source"
        }
    let sinks     = \x -> case x of
        { "tempo"         -> tempoS
        ; "gain"          -> gainS
        ; "volume"        -> volumeS
        ; "transport"     -> transportS
        ;  _              -> error "No such sink"
        }
    return (sources, sinks)


addTimers :: Frame a -> IO (String -> Event Int, String -> Sink ())
addTimers frame = do
    (timerFired, timerFiredE) <- newSource

    timer frame [interval := 2000,
                on command := timerFired 0]

    let sources = \x -> case x of { "fired" -> timerFiredE }
    let sinks   = error "No such sink"
    return (sources, sinks)


gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]

    (menuSources,   menuSinks)   <- addMenus frame
    (widgetSources, widgetSinks) <- addWidgets frame
    (timerSources,  timerSinks)  <- addTimers frame

    let 
        -- timeR :: Reactive Double
        -- timeR = accumR 0 ((+ 0.05) <$ pulse 0.05)
        timeR = time

        startE, stopE, pauseE, resumeE :: Event ()
        startE  = tickE $ widgetSources "start"
        stopE   = tickE $ widgetSources "stop"
        pauseE  = tickE $ widgetSources "pause"
        resumeE = tickE $ widgetSources "resume"

        tempoR, gainR, volumeR :: Reactive Double
        tempoR  = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "tempo"
        gainR   = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "gain"
        volumeR = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "volume"

        transportS :: Sink Int
        transportS = widgetSinks "transport"
        
        gainS :: Sink Int
        gainS = widgetSinks "gain"

        writeGain x      = gainS $ (round <$> x * 1000.0)       `sample` pulse 0.1
        writeTransport x = transportS $ (round <$> x * 1000.0)  `sample` pulse 0.1
        -- TODO write server status etc

        -- --------------------------------------------------------
        
        control :: Event (Transport Double)
        control = mempty 
            <> (Play    <$ startE) 
            <> (Pause   <$ pauseE) 
            <> (Stop    <$ stopE) 
            <> (Reverse <$ resumeE)

        duration :: Reactive Double
        duration = (1*60)                               
        
        position :: Reactive Double
        position  = transport control (pulse 0.1) (tempoR * 10) / duration

        actions :: Event String
        actions = playback position $ pure 
            [ 
                (0.0, "What"),
                (0.1, "a"),
                (0.2, "day"),
                (0.3, "for"),
                (0.4, "an"),            
                (0.5, "Auto-da-fe")            
            ]
        
    -- --------------------------------------------------------
    eventLoop <- return $ runLoopUntil $ mempty
        <> (continue $ writeTransport position)
        <> (continue $ showing "Events:   " $ actions)
        -- <> (continue $ showing "Speed:    " $ tempoR `sample` pulse 0.1)
        -- <> (continue $ showing "Position: " $ position `sample` pulse 0.1)

    -- --------------------------------------------------------
    forkIO eventLoop
    return ()





main :: IO ()
main = start gui

-- wxhaskell extra
set' :: w -> Attr w a -> Maybe a -> IO ()
set' widget prop x = case x of
    Just x  -> set widget [prop := x]
    Nothing -> return ()

continue :: Event a -> Event (Maybe b)
continue     = (Nothing <$)
