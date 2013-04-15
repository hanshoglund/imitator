
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.VectorSpace
import Control.Applicative
import Control.Monad (join)
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Reactive
import Control.Reactive.Chan
import Control.Reactive.Midi
import Control.Reactive.Osc
import System.IO.Unsafe
import System.Exit

import Graphics.UI.WX hiding (Event, Reactive)

import Music.Score (Time(..))
import Music.Imitator hiding (text)

import Score

addMenus :: Frame a -> IO (String -> Event Int, String -> Sink ())
addMenus frame = do
    file            <- menuPane [text := "&File"]
    fileOpen        <- menuItem file [text := "&Open...\tCtrl+O"]
    menuLine file
    fileSave        <- menuItem file [text := "&Save\tCtrl+S"]
    fileSaveAs      <- menuItem file [text := "&Save As...\tCtrl+Shift+S"]
    menuLine file
    fileQuit        <- menuItem file [text := "&Quit\tCtrl+Q"]

    transport          <- menuPane [text := "&Transport"]
    transportStart     <- menuItem transport [text := "&Start\tCtrl+R"]
    transportPause     <- menuItem transport [text := "&Pause\tCtrl+P"]
    transportAbort     <- menuItem transport  [text := "&Abort\tCtrl+A"]
    transportStop      <- menuItem transport [text := "&Stop\tCtrl+S"]

    -- window          <- menuPane [text := "&Window"]
    -- windowMinimize  <- menuItem window [text := "&Minimize\tCtrl+M"]
    -- windowZoom      <- menuItem window [text := "&Zoom"]

    set frame [
        menuBar            := [file, transport{-, window-}],
        on (menu fileQuit) := close frame,
        on (menu transportStart) := return ()
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
    set transportStart   [on command := startA 0]
    set transportStop    [on command := stopA 0]
    set transportPause   [on command := pauseA 0]
    set transportAbort   [on command := abortA 0]

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

    cpu           <- textEntry frame [enabled := False]
    memory        <- textEntry frame [enabled := False]
    server        <- textEntry frame [enabled := False]
    serverMeanCpu <- textEntry frame [enabled := False]
    serverPeakCpu <- textEntry frame [enabled := False]

    transport   <- hgauge frame 1000 [text := "Volume", size := sz 750 30]

    -- Set layout
    let buttons = margin 10 $ boxed "Transport" $
            grid 10 10 [ [widget start, widget pause],
                         [widget stop, widget abort] ]

        controls  = margin 10 $ boxed "Controls" $
            grid 10 5 [ [label "Tempo:", widget tempo],
                        [label "Gain:", widget gain],
                        [label "Volume:", widget volume] ]

        -- const (-1)
        -- const (-1)
        -- isServerRunning
        -- serverCPUAverage
        -- serverCPUPeak
        status = margin 10 $ boxed "Status" $
            grid 0 0 [
                [label "CPU (%):",              widget cpu],
                [label "Memory (MB):",          widget memory],
                [label "Server:",               widget server],
                [label "Server mean CPU (%):",  widget serverMeanCpu],
                [label "Server peak CPU (%):",  widget serverPeakCpu]
            ]

        positioning = shaped $ margin 10 $ column 10 [
            widget transport,
            row 10 [
                label "Time:", 
                label "Section:", 
                label "Bar:"]
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

    (cpuB, cpuS)                        <- newSink
    (memoryB, memoryS)                  <- newSink
    (serverB, serverS)                  <- newSink
    (serverMeanCpuB, serverMeanCpuS)    <- newSink
    (serverPeakCpuB, serverPeakCpuS)    <- newSink

    set start   [on command := startA 0]
    set stop    [on command := stopA 0]
    set pause   [on command := pauseA 0]
    set abort   [on command := abortA 0]

    set tempo   [on command := get tempo  selection >>= tempoA]
    set gain    [on command := get gain   selection >>= gainA]
    set volume  [on command := get volume selection >>= volumeA]

    let refreshControls = do
        tempoB      >>= set' tempo selection
        gainB       >>= set' gain selection
        volumeB     >>= set' volume selection
        transportB  >>= set' transport selection
        return ()

    let refreshServerStatus = do
        cpuB           >>= (set' cpu text . fmap (show . (/ 1000) . toDouble))
        memoryB        >>= (set' memory text . fmap (show . (/ 1000) . toDouble))
        serverB        >>= (set' server text . fmap (\x -> if (x > 0) then "Running" else "Stopped"))
        serverMeanCpuB >>= (set' serverMeanCpu text . fmap (show . (/ 1000) . toDouble))
        serverPeakCpuB >>= (set' serverPeakCpu text . fmap (show . (/ 1000) . toDouble))        
        return ()

    -- FIXME should match pulses below
    timer frame [interval := 20, on command := refreshControls]
    timer frame [interval := 20, on command := refreshServerStatus]

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
        { "tempo"           -> tempoS
        ; "gain"            -> gainS
        ; "volume"          -> volumeS
        ; "transport"       -> transportS
        ; "cpu"             -> cpuS
        ; "memory"          -> memoryS
        ; "server"          -> serverS
        ; "serverMeanCpu"   -> serverMeanCpuS
        ; "serverPeakCpu"   -> serverPeakCpuS
        ;  _                -> error "No such sink"
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
    threadDelay 500000
    frame <- frame [text := "Imitator"]

    (menuSources,   menuSinks)   <- addMenus frame
    (widgetSources, widgetSinks) <- addWidgets frame
    (timerSources,  timerSinks)  <- addTimers frame

    let 
        -- | Transport events from GUI
        startE, stopE, pauseE, abortE :: Event ()
        startE  = tickE $ widgetSources "start" <> menuSources "start"
        stopE   = tickE $ widgetSources "stop"  <> menuSources "stop"
        pauseE  = tickE $ widgetSources "pause" <> menuSources "pause"
        abortE  = tickE $ widgetSources "abort" <> menuSources "abort"
        quitE   = tickE $ menuSources "quit"

        -- | Control inputs from GUI
        tempoR, gainR, volumeR :: Reactive Double
        tempoE  = scaleTempo . (/ 1000) . fromIntegral <$> widgetSources "tempo"
        tempoR  = initTempo `stepper` tempoE
        gainR   = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "gain"
        volumeR = (/ 1000) . fromIntegral <$> 0 `stepper` widgetSources "volume"

        -- Tempo interpolation is linear from 0.2 to 1.8
        scaleTempo x   = 0.8*(x*2-1) + 1

        -- | Transport and gain to GUI
        transportS, gainS :: Sink Double
        transportS = widgetSinks "transport" . (round . (* 1000) <$>)
        tempoS     = widgetSinks "tempo"     . (round . (* 1000) <$>)
        gainS      = widgetSinks "gain"      . (round . (* 1000) <$>)

        -- | Server status to GUI
        cpuS, memoryS, serverS, serverMeanCpuS, serverPeakCpuS :: Sink Double
        cpuS            = widgetSinks "cpu"           . (round . (* 1000.0) <$>)
        memoryS         = widgetSinks "memory"        . (round . (* 1000.0) <$>)
        serverS         = widgetSinks "server"        . (round . (* 1000.0) <$>)
        serverMeanCpuS  = widgetSinks "serverMeanCpu" . (round . (* 1000.0) <$>)
        serverPeakCpuS  = widgetSinks "serverPeakCpu" . (round . (* 1000.0) <$>)

        transportPulse, serverStatusPulse :: Event ()
        transportPulse    = pulse 0.05
        serverStatusPulse = pulse 10
        
        initTempo :: Double
        initTempo = 1

        -- | Commands to server
        commandsS :: Event OscMessage -> Event OscMessage
        commandsS msgs = oscOutUdp "127.0.0.1" 57110 $ msgs         

        control :: Event (TransportControl Time)
        control = mempty 
            <> (Play    <$ startE) 
            <> (Pause   <$ pauseE) 
            <> (Stop    <$ stopE) 

        -- | Advances in seconds at tempo 1
        --   Should go from 0 to totalDur during performance 
        absPos :: Reactive Time
        absPos  = transport control transportPulse (toTime <$> tempoR)

        -- | Goes from 0 to 1 during performance
        relPos :: Reactive Time
        relPos = absPos ^/ totalDur
        totalDur = offset cmdScore

       
        serverMessages :: Event OscMessage
        serverMessages = imitatorRT (scoreToTrack cmdScore) absPos
         
    -- --------------------------------------------------------
    eventLoop <- return $ runLoopUntil $ mempty

{-
        <> (continue $ cpuS             $ pure 0.0          `sample` serverStatusPulse)
        <> (continue $ memoryS          $ pure 0.0          `sample` serverStatusPulse)
        <> (continue $ serverS          $ pure 1.0          `sample` serverStatusPulse)
        <> (continue $ serverMeanCpuS   $ serverCPUAverage  `sample` serverStatusPulse)
        <> (continue $ serverPeakCpuS   $ serverCPUPeak     `sample` serverStatusPulse)
-}
        
        <> (continue $ showing "Sending: "   $ commandsS  $ serverMessages)
        <> (continue                         $ transportS $ fromTime <$> relPos `sample` transportPulse)
        <> (continue $ notify  "Quitting "   $ putE (const $ close frame) $ quitE)
        <> (continue $ notify  "Aborting "   $ putE (const $ abort) $ abortE)

        <> (continue $ showing "Position: "  $ fmap toDouble $ absPos `sample` transportPulse)
        -- <> (continue $ showing "Tempo: "     $ fmap toDouble $ tempoR `sample` tempoE)

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

toTime :: Real a => a -> Time
toTime = Time . toRational

fromTime = fromRational . getTime

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

once :: a -> Event a
once x = unsafePerformIO $ do
    (k,s) <- newSource
    k x
    return s

-- once x = (const x) <$> firstE (pulse 0.000000001)

