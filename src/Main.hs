
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Control.Applicative

import Control.Concurrent (forkIO, forkOS, threadDelay)
import System.Exit

import Graphics.UI.WX hiding (Event)

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
    (startA, startE)            <- newSourceE
    (stopA, stopE)              <- newSourceE
    (pauseA, pauseE)            <- newSourceE
    (resumeA, resumeE)          <- newSourceE
    (tempoA, tempoE)            <- newSourceE
    (gainA, gainE)              <- newSourceE
    (volumeA, volumeE)          <- newSourceE

    (tempoB, tempoS)            <- newSinkE
    (gainB, gainS)              <- newSinkE
    (volumeB, volumeS)          <- newSinkE
    (transportB, transportS)    <- newSinkE

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
    (timerFired, timerFiredE) <- newSourceE

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
        startE = widgetSources "start"
        stopE = widgetSources "stop"
        pauseE = widgetSources "pause"
        tempoE = widgetSources "tempo"
        gainE = widgetSources "gain"
        
        transportS = widgetSinks "transport"
        
        tempoR = stepper 0 tempoE
        gainR = stepper 0 gainE
        startClicksR = accumR 0 (fmap (const (+ 1)) startE)
        startClicksE = accumE 0 (fmap (const (+ 1)) startE)
        
        continueE = fmap (const Nothing)

    -- TODO split into something run by a timer
    eventLoop <- return $ runLoopUntil $ neverE
        -- <> (continueE $ notify "Foo" $ mempty)
        -- <> (continueE $ notify "Bar" $ mempty <> mempty <> startE <> mempty <> mempty)
        -- <> (continueE $ notify "Baz" $ (startE <> mempty) <> (mempty <> pauseE))

        -- <> (continueE $ showing "tempo:         "    $ sample tempoR startE)
        -- <> (continueE $ showing "gain:          "    $ sample gainR  stopE)
        -- <> (continueE $ showing "tempo + gain:  "    $ sample (liftA2 (+) tempoR gainR) pauseE)
        <> (continueE $ showing "Start clicks: "     $ startClicksE)
        <> (continueE $ showing "Prev start clicks: " $ delayE 3 startClicksE)


        -- <> (continueE $ transportS $ fmap (* 10) $ startClicksE)
        -- <> (continueE $ filterE (> 20) startClicksE)

        -- <> (continueE $ notify "Start was pressed"   $ widgetSources "start")
        -- <> (continueE $ notify "Stop was pressed"    $ widgetSources "stop")
        -- <> (continueE $ notify "Pause was pressed"   $ widgetSources "pause")
        -- <> (continueE $ notify "Resume was pressed"  $ widgetSources "resume")
        -- <> (continueE $ showing "Tempo is now: "     $ widgetSources "tempo")
        -- <> (continueE $ showing "Gain is now: "     $ widgetSources "gain")
        -- <> (continueE $ showing "Volume is now: "     $ widgetSources "volume")

        -- <> (continueE $ showing "Tempo + Gain: "     $ liftA2 (+) (widgetSources "tempo") (widgetSources "gain"))


        -- <> (continueE $ showing "Entered text reversed: " $ fmap reverse $ getLineE)
        -- <> (continueE $ widgetSinks "tempo" $ fmap (const 500)     $ widgetSources "stop")
        -- <> (continueE $ widgetSinks "transport" $ widgetSources "volume")

    forkIO eventLoop
    return ()

main :: IO ()
main = start gui

-- wxhaskell extra
set' :: w -> Attr w a -> Maybe a -> IO ()
set' widget prop x = case x of
    Just x  -> set widget [prop := x]
    Nothing -> return ()










