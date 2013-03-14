
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Music.Imitator.Reactive
import Control.Concurrent (forkIO, forkOS, threadDelay)

import System.Exit
import Data.Monoid
import Control.Applicative
import Graphics.UI.WX hiding (Event)

addMenus :: Frame a -> IO (String -> Event (), String -> Listener ())
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
    
    let events    = error "No such event"
    let listeners = error "No such listener"
    return (events, listeners)    



addWidgets :: Frame a -> IO (String -> Event (), String -> Listener ())
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
    set tempo  [on command := putStrLn  "tempo changed!!!"]
    set gain   [on command := putStrLn  "gain changed!!!"]
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

    let events    = error "No such event"
    let listeners = error "No such listener"
    return (events, listeners)    


addTimers :: Frame a -> IO (String -> Event (), String -> Listener ())
addTimers frame = do
    (timerFired, timerFiredE) <- newObsE

    timer frame [interval := 2000,
                on command := timerFired ()]

    let events = \x -> case x of { "fired" -> timerFiredE }
    let listeners = error "No such listener"
    return (events, listeners)    


gui :: IO ()
gui = do
    frame <- frame [text := "Imitator"]

    (menuEvents,   menuListeners)   <- addMenus frame
    (widgetEvents, widgetListeners) <- addWidgets frame
    (timerEvents,  timerListeners)  <- addTimers frame
                                  
    forkIO $ runLoop $ putLineE $ fmap (const "The timer was fired") $ timerEvents "fired"
    return ()

type Listener a = Event a -> Event a

main :: IO ()
main = start gui
-- main = eventMain mainE


newObsE :: IO (a -> IO (), Event a)
newObsE = do
    ch <- newChan
    return (writeChan ch, readChanE ch)


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



-- data E a = E a {-...-}
-- instance Monoid E where
-- 
-- data B a = B a {-...-}
-- instance Monoid a => Monoid (B a) where
--     mempty  = pure mempty        
--     mappend = liftA2 mappend
-- instance Functor (B a) where
--     fmap f  = (pure f <*>)
-- instance Applicative (B a) where
--     pure x  = undefined         -- const x
--     f <*> x = undefined         -- (f t) (f x)
-- instance Monad (B a) where
--     return = pure
--     x >>= k = undefined         -- 

{-
    Functor: 
        at (fmap f r) == fmap f (at r)
    Applicative: 
        at (pure a)  == pure a
        at (s <*> r) == at s <*> at t
    Monad: 
        at (return a) == return a
        at (join rr) == join (at . at rr). 
        (r >>= f) == join (fmap f r)
        at (r >>= f) == at r >>= at . f.
    Monoid: a typical lifted monoid. If o is a monoid, then Reactive o is a monoid, with mempty == pure mempty, 
    and mappend == liftA2 mappend. That is, mempty at t == mempty, and (r mappend s) at t == (r at t) mappend (s at t). 

    

-}




-- data Action 
--     = ButtonAction
--     | SliderAction
--     | MenuAction
--     | TimerAction 
-- 
-- data Update
--     = ButtonUpdate
--     | SliderUpdate
--     | MenuUpdate
--     | LabelUpdate
--     | GaugeUpdate
--                     
        
    






