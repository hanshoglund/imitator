
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Music.Imitator.Reactive (
        Chan,
        newChan,
        dupChan,
        writeChan,
        readChan,
        peekChan,
        tryReadChan,
        tryPeekChan,
        Event,
        filterE,
        readE,
        writeE,
        getE,
        putE,
        -- MidiSource,
        -- MidiDestination,
        -- midiInE,
        -- midiOutE,
        -- OscMessage,
        -- oscInE,
        -- oscOutE,
        linesIn,
        linesOut, 
        run,
        runLoop
  ) where

import Data.Monoid  
import Data.Maybe
import Data.Traversable
import System.IO.Unsafe

import Control.Newtype
import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan


import System.MIDI (MidiMessage,  MidiMessage')
import qualified System.MIDI            as Midi
import qualified Sound.OpenSoundControl as OSC

-- kPortMidiInfo = unsafePerformIO $ do
--     Midi.initialize
--     num  <- Midi.countDevices
--     infos <- Prelude.mapM Midi.getDeviceInfo [0..num - 1]
--     return infos     


-- Factor out channels
newtype Chan a = Chan { getChan :: TChan a }
newChan     :: IO (Chan a)
dupChan     :: Chan a -> IO (Chan a)
writeChan   :: Chan a -> a -> IO ()
readChan    :: Chan a -> IO a
peekChan    :: Chan a -> IO a
tryReadChan :: Chan a -> IO (Maybe a)
tryPeekChan :: Chan a -> IO (Maybe a)
newChan       = atomically . fmap Chan $ newTChan
dupChan c     = atomically . fmap Chan $ dupTChan (getChan c)
writeChan c   = atomically . writeTChan (getChan c)
readChan      = atomically . readTChan . getChan
peekChan      = atomically . peekTChan . getChan
tryReadChan   = atomically . tryReadTChan . getChan
tryPeekChan   = atomically . tryPeekTChan . getChan


newtype Event a = Event { getEvent :: IO [a] }

instance Functor Event where
    fmap f = Event . (fmap (fmap f)) . getEvent

filterE :: (a -> Bool) -> Event a -> Event a
filterE p (Event f) = Event $ do
    x <- f
    case x of
        [] -> return []
        xs -> return (filter p xs)

instance Monoid (Event a) where
    mempty = Event $ return []
    Event f `mappend` Event g = Event $ do
        x <- f
        y <- g
        return (x ++ y)


getE :: IO (Maybe a) -> Event a
getE = Event . fmap maybeToList

putE :: (a -> IO ()) -> Event a -> Event a
putE g (Event f) = Event $ do
    x <- f
    case x of
        [] -> return []
        xs -> Prelude.mapM g xs
    return x

readE :: Chan a -> Event a
readE ch = getE (tryReadChan ch)

writeE :: Chan a -> Event a -> Event a
writeE ch e = putE (writeChan ch) e

-- TODO make non-blocking    
linesIn :: [Event String]
linesIn = unsafePerformIO $ do
    ch <- newChan
    forkIO $ cycleM $ do
        getLine >>= writeChan ch
    fmap (fmap readE) $ Prelude.mapM dupChan (replicate 10 ch)
    where
        cycleM x = x >> cycleM x



linesOut :: Event String -> Event String
linesOut = putE putStrLn

-- |
-- Run an event, distributing a single occurance if there is one.
-- 
-- This may result in wrapped actions being executed. 
-- If more than one event refer to a single channel they compete for its contents (i.e. non-determinism).
--
run :: Event a -> IO ()
run (Event f) = do
    x <- f
    return ()

runLoop :: Event a -> IO ()
runLoop e = run e >> threadDelay kloopInterval >> runLoop e  

kloopInterval = 1000 * 5




-- 
-- type MidiSource      = Midi.Source
-- type MidiDestination = Midi.Destination
-- 
-- midiInE :: MidiSource -> Event MidiMessage
-- midiInE = undefined
-- 
-- midiOutE :: MidiDestination -> Event MidiMessage -> Event MidiMessage
-- midiOutE = undefined
-- 
-- type OscMessage = OSC.Message
-- 
-- oscInE :: Int -> Event OscMessage
-- oscInE = undefined
-- 
-- oscOutE :: String -> Int -> Event OscMessage
-- oscOutE = undefined
