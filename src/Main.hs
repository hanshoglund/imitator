
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Data.Traversable
import Control.Concurrent.Chan
import Graphics.UI.WX

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
        on (menu fileQuit) := close frame,
        on (menu recordStart) := putStrLn "Started recording..."
        ]

addWidgets :: Frame a -> IO (Gauge ())
addWidgets frame = do
    start       <- button frame [text := "Start"]
    stop        <- button frame [text := "Stop"]
    pause       <- button frame [text := "Pause"]
    resume      <- button frame [text := "Resume"]
    set start [on command := putStrLn  "start pressed!!!"]
    set stop [on command := putStrLn   "stop pressed!!!"]
    set pause [on command := putStrLn  "pause pressed!!!"]
    set resume [on command := putStrLn "resume pressed!!!"]
    
    tempo       <- hslider frame True 0 1000 [text := "Tempo"]
    gain        <- hslider frame True 0 1000 [text := "Gain"]
    volume      <- hslider frame True 0 1000 [text := "Volume"]
    set tempo [on command := putStrLn  "tempo changed!!!"]
    set gain [on command := putStrLn   "gain changed!!!"]
    set volume [on command := putStrLn  "volume changed!!!"]

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

    return transport

addTimers :: Gauge () -> Frame a -> IO ()
addTimers transport frame = do
    timer frame [interval := 2000,
                on command := fireTimer]
    return ()                          
    where
        fireTimer = do
            putStrLn "timer fired!!!"
            set transport [selection := 500]


gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]

    addMenus frame
    transport <- addWidgets frame
    addTimers transport frame
    return ()

-- main :: IO ()
-- main = start gui

-- midiIn :: Chan Midi
-- midiOut :: Chan Midi
-- guiIn :: Chan GuiActions
-- guiOut :: Chan GuiUpdates
-- engineOut :: Chan Command




data EvVal a = EvVal a | EvWait | EvDone

instance Functor EvVal where
    fmap f (EvVal x) = EvVal (f x)
    fmap f EvWait    = EvWait
    fmap f EvDone    = EvDone
    
instance Monad EvVal where
    return = EvVal               
    x >>= f = join' . fmap f $ x
        where               
            join' :: EvVal (EvVal a) -> EvVal a
            join' (EvVal x) = x
            join' EvWait    = EvWait
            join' EvDone    = EvDone

newtype Ev a = Ev { getEv :: (IO (EvVal a)) }

instance Functor Ev where
    fmap f (Ev g) = Ev $ do
        x <- g
        return $ fmap f x

instance Monad Ev where
    return = Ev . return . return
    (Ev f) >>= k = Ev $ do
        x <- f
        case x of
            (EvVal x) -> (getEv . k) x
            EvWait    -> return EvWait
            EvDone    -> return EvDone

readChanE :: Chan a -> Ev a
readChanE ch = Ev $ do
    st <- isEmptyChan ch
    if st then do
            x <- readChan ch
            return $ return x
        else
            return $ EvWait

writeChanE :: Chan a -> Ev a -> Ev a
writeChanE ch (Ev f) = Ev $ do
    x <- f
    case x of
        (EvVal x) -> writeChan ch x
        _         -> return ()
    return x
    
linesIn  :: Ev String
linesIn = Ev $ fmap EvVal getLine

linesOut :: Ev String -> Ev String
linesOut (Ev f) = Ev $ do
    x <- f
    case x of
        (EvVal x) -> putStrLn x
        _         -> return ()
    return x

run :: Ev a -> IO ()
run (Ev f) = do
    x <- f
    return ()

-- TODO break on done
runLoop :: Ev a -> IO ()
runLoop e = run e >> runLoop e


-- run :: (State Action -> State Update) -> IO ()
data Action 
    = ButtonAction
    | SliderAction
    | MenuAction
    | TimerAction 

data Update
    = ButtonUpdate
    | SliderUpdate
    | MenuUpdate
    | LabelUpdate
    | GaugeUpdate
        
        
    






