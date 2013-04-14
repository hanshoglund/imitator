
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Control.Applicative
import Control.Monad (join)
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Reactive
import Control.Reactive.Chan
import Control.Reactive.Midi
import Control.Reactive.Osc
import System.Exit

import Graphics.UI.WX hiding (Event, Reactive)

import Music.Score (Time(..))
import Music.Imitator hiding (text)

import Score

addMenus :: Frame a -> IO (String -> Event Int, String -> Sink ())
addMenus frame = do
    file            <- menuPane [text := "&File"]
    fileOpen        <- menuItem file [text := "&Open...\tCtrl+O"]
    fileQuit        <- menuItem file [text := "&Quit\tCtrl+Q"]

    record          <- menuPane [text := "&Record"]
    recordStart     <- menuItem record [text := "&Start\tCtrl+R"]
    recordPause     <- menuItem record [text := "&Pause\tCtrl+P"]
    recordAbort    <- menuItem record  [text := "&Abort\tCtrl+A"]
    recordStop      <- menuItem record [text := "&Stop\tCtrl+S"]

    window          <- menuPane [text := "&Window"]
    windowMinimize  <- menuItem window [text := "&Minimize\tCtrl+M"]
    windowZoom      <- menuItem window [text := "&Zoom"]

    set frame [
        menuBar            := [file, record, window],
        on (menu fileQuit) := close frame,
        on (menu recordStart) := return ()
        ]

    -- Create sources/sinks
    (startA, startE)            <- newSource
    (stopA, stopE)              <- newSource
    (pauseA, pauseE)            <- newSource
    (abortA, abortE)            <- newSource
    (openA, openE)              <- newSource
    (quitA, quitE)              <- newSource
    (volumeA, volumeE)          <- newSource

    set fileOpen      [on command := openA 0]
    set fileQuit      [on command := quitA 0]
    set recordStart   [on command := startA 0]
    set recordStop    [on command := stopA 0]
    set recordPause   [on command := pauseA 0]
    set recordAbort   [on command := abortA 0]

    let sources     = \x -> case x of
        { "open"          -> openE
        ; "quit"          -> quitE
        ; "start"         -> startE
        ; "stop"          -> stopE
        ; "pause"         -> pauseE
        ; "abort"         -> abortE
        ;  _              -> error "No such source"
        }                          
    let sinks       = error "No such sink"
    return (sources, sinks)



addWidgets :: Frame a -> IO (String -> Event Int, String -> Sink Int)
addWidgets frame = do
    
    -- Create widgets
    start       <- button frame [text := "Start"]
    stop        <- button frame [text := "Stop"]
    pause       <- button frame [text := "Pause"]
    abort       <- button frame [text := "Abort"]

    tempo       <- hslider frame True 0 1000 [text := "Tempo"]
    gain        <- hslider frame True 0 1000 [text := "Gain"]
    volume      <- hslider frame True 0 1000 [text := "Volume"]

    transport   <- hgauge frame 1000 [text := "Volume", size := sz 750 30]

    -- Set layout
    let buttons = margin 10 $ boxed "Transport" $
            grid 10 10 [ [widget start, widget pause],
                         [widget stop, widget abort] ]

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
    (abortA, abortE)            <- newSource
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
    set abort   [on command := abortA 0]

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
        ; "abort"         -> abortE
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
    startServer
    writeSynthDefs
    frame <- frame [text := "Imitator"]

    (menuSources,   menuSinks)   <- addMenus frame
    (widgetSources, widgetSinks) <- addWidgets frame
    (timerSources,  timerSinks)  <- addTimers frame

    let 
        startE, stopE, pauseE, abortE :: Event ()
        startE  = tickE $ widgetSources "start" <> menuSources "start"
        stopE   = tickE $ widgetSources "stop"  <> menuSources "stop"
        pauseE  = tickE $ widgetSources "pause" <> menuSources "pause"
        abortE  = tickE $ widgetSources "abort" <> menuSources "abort"
        quitE   = tickE $ menuSources "quit"

        tempoR, gainR, volumeR :: Reactive Double
        tempoR  = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "tempo"
        gainR   = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "gain"
        volumeR = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "volume"

        transportS :: Sink Double
        transportS = widgetSinks "transport" . (round . (* 1000.0) <$>)
        
        gainS :: Sink Double
        gainS = widgetSinks "gain" . (round . (* 1000.0) <$>)
        
        serverS :: Event OscMessage -> Event OscMessage
        serverS msgs = oscOutUdp "127.0.0.1" 57110 $ msgs         

        -- TODO write server status etc

        control :: Event (TransportControl Time)
        control = mempty 
            <> (Play    <$ startE) 
            <> (Pause   <$ pauseE) 
            <> (Stop    <$ stopE) 

        duration :: Reactive Time
        duration = (1*60)                               
        
        position :: Reactive Time
        position  = transport control (pulse 0.1) ((Time . toRational <$> tempoR) * 10) / duration

        serverMessages :: Event OscMessage
        serverMessages = imitatorRT (scoreToTrack cmdScore) (position * 100)
         
    -- --------------------------------------------------------
    eventLoop <- return $ runLoopUntil $ mempty

        <> (continue $ transportS  $ (fromRational . getTime) <$> position `sample` pulse 0.1)
        <> (continue $ serverS     $ serverMessages)

        -- TODO
        <> (continue $ notify "Quitting " $ putE (const $ close frame) $ quitE)
        <> (continue $ notify "Aborting " $ putE (const $ abort) $ abortE)

        <> (continue $ showing "Sending to server:  " $ serverMessages)

    -- --------------------------------------------------------

    forkIO eventLoop
    return ()


main :: IO ()
main = do
    start gui   -- blocking until GUI finishes
    stopServer

-- wxhaskell extra
set' :: w -> Attr w a -> Maybe a -> IO ()
set' widget prop x = case x of
    Just x  -> set widget [prop := x]
    Nothing -> return ()

continue   :: Event a -> Event (Maybe b)
noContinue :: Event a -> Event (Maybe a)
continue   = (Nothing <$)
noContinue = (Just <$>)

fromJust :: Maybe a -> a
fromJust (Just x) = x
