
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Music.Imitator.Reactive
import Control.Concurrent (forkIO, forkOS, threadDelay)

import System.Exit
import Data.Monoid
import Control.Applicative
import Graphics.UI.WX hiding (Event)

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

    let events    = error "No such event"
    let sinks = error "No such sink"
    return (events, sinks)



addWidgets :: Frame a -> IO (String -> Event Int, String -> Sink Int)
addWidgets frame = do
    (startA, startE) <- newObsE
    (stopA, stopE) <- newObsE
    (pauseA, pauseE) <- newObsE
    (resumeA, resumeE) <- newObsE
    (tempoA, tempoE) <- newObsE
    (gainA, gainE) <- newObsE
    (volumeA, volumeE) <- newObsE

    start       <- button frame [text := "Start"]
    stop        <- button frame [text := "Stop"]
    pause       <- button frame [text := "Pause"]
    resume      <- button frame [text := "Resume"]
    set start [on command  := startA 0]
    set stop [on command   := stopA 0]
    set pause [on command  := pauseA 0]
    set resume [on command := resumeA 0]

    tempo       <- hslider frame True 0 1000 [text := "Tempo"]
    gain        <- hslider frame True 0 1000 [text := "Gain"]
    volume      <- hslider frame True 0 1000 [text := "Volume"]
    set tempo  [on command := get tempo  selection >>= tempoA]
    set gain   [on command := get gain   selection >>= gainA]
    set volume [on command := get volume selection >>= volumeA]

    transport <- hgauge frame 1000 [text := "Volume", size := sz 750 30]

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

    (tempoB, tempoS) <- newSinkE
    (gainB, gainS) <- newSinkE
    (volumeB, volumeS) <- newSinkE
    (transportB, transportS) <- newSinkE

    let refreshWidgets = do
        tempoB      >>= setProp tempo selection
        gainB       >>= setProp gain selection
        volumeB     >>= setProp volume selection
        transportB  >>= setProp transport selection
        return ()

    timer frame [interval := 100, on command := refreshWidgets]



    let events     = \x -> case x of
        { "start"         -> startE
        ; "stop"          -> stopE
        ; "pause"         -> pauseE
        ; "resume"        -> resumeE
        ; "tempo"         -> tempoE
        ; "gain"          -> gainE
        ; "volume"        -> volumeE
        ;  _              -> error "No such event"
        }
    let sinks     = \x -> case x of
        { "tempo"         -> tempoS
        ; "gain"          -> gainS
        ; "volume"        -> volumeS
        ; "transport"     -> transportS
        ;  _              -> error "No such sink"
        }
    return (events, sinks)

setProp :: w -> Attr w a -> Maybe a -> IO ()
setProp widget prop x = case x of
    Just x  -> set widget [prop := x]
    Nothing -> return ()

addTimers :: Frame a -> IO (String -> Event Int, String -> Sink ())
addTimers frame = do
    (timerFired, timerFiredE) <- newObsE

    timer frame [interval := 2000,
                on command := timerFired 0]

    let events = \x -> case x of { "fired" -> timerFiredE }
    let sinks = error "No such sink"
    return (events, sinks)


gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]

    (menuEvents,   menuSinks)   <- addMenus frame
    (widgetEvents, widgetSinks) <- addWidgets frame
    (timerEvents,  timerSinks)  <- addTimers frame

    -- TODO split into something run by a timer
    forkIO $ runLoop $ mempty
        <> (notify "Start was pressed"   $ widgetEvents "start")
        <> (notify "Stop was pressed"    $ widgetEvents "stop")
        <> (notify "Pause was pressed"   $ widgetEvents "pause")
        <> (notify "Resume was pressed"  $ widgetEvents "resume")
        <> (showing "Tempo is now:"      $ widgetEvents "tempo")
        <> (notify "The timer was fired" $ timerEvents  "fired")
        <> (notify "Something happened"  $ widgetEvents "start" <> timerEvents "fired")

        <> (fmap (const "") $ widgetSinks "transport" $ fmap (const 500) $ widgetEvents "resume")
        <> (fmap (const "") $ widgetSinks "tempo" $ fmap (const 500)     $ widgetEvents "stop")


    return ()

type Sink a = Event a -> Event a

notify :: String -> Event a -> Event String
notify m = putLineE . fmap (const m)

showing :: Show a => String -> Event a -> Event String
showing m = putLineE . fmap (\x -> m ++ show x)


main :: IO ()
main = start gui
-- main = eventMain mainE


newObsE :: IO (a -> IO (), Event a)
newObsE = do
    ch <- newChan
    return (writeChan ch, readChanE ch)

newSinkE :: IO (IO (Maybe a), Sink a)
newSinkE = do
    ch <- newChan
    return (tryReadChan ch, writeChanE ch)


-- combo box, text etc...
    -- command :: IO ()        -- set to decide action
    -- get     :: IO a         -- use on action to get value...
    -- set     :: a -> IO ()   -- use to set value

-- gauge etc...
    -- set     :: a -> IO ()   -- use to set value
    -- get     :: IO a         -- use to get value

-- timer, button...
    -- command :: IO ()        -- set to decide action


-- mainE :: Event (Maybe Bool)
-- mainE = output `sequenceE` result
--     where
--         result       = fmap (\x -> if (x == "exit") then Just True else Nothing) getLineE
--         output       = putLineE $ twice
--         twice        = yourText "(original)" getLineE <> yourText "(reversed)" (fmap reverse getLineE)
--
--         yourText t = mergeWithE (++) (alwaysE $ "Your text " ++ t ++ ": ")
--
--
--
--
--
-- eventMain :: Event (Maybe Bool) -> IO ()
-- eventMain = eventMain' . (fmap . fmap) (\r -> if r then ExitSuccess else ExitFailure (-1))
--
-- eventMain' :: Event (Maybe ExitCode) -> IO ()
-- eventMain' e = do
--     code <- runLoopUntil e
--     exitWith code

-- midiIn :: Chan Midi
-- midiOut :: Chan Midi
-- guiIn :: Chan GuiActions
-- guiOut :: Chan GuiUpdates
-- engineOut :: Chan Command










