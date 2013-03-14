
{-# LANGUAGE GADTs #-}

module Music.Imitator.Reactive (
        Chan,
        newChan,
        dupChan,
        writeChan,
        tryReadChan,
        Event,          
        neverE,
        mergeE,
        -- filterE,
        -- readE,
        -- writeE,
        -- -- MidiSource,
        -- -- MidiDestination,
        -- -- midiInE,
        -- -- midiOutE,
        -- -- OscMessage,
        -- -- oscInE,
        -- -- oscOutE,
        linesIn,
        linesOut, 
        getE,
        getNonBlockingE,
        putE,
        readE,
        writeE,
        run,
        runLoop
  ) where

import Prelude hiding (mapM)

import Data.Monoid  
import Data.Maybe
import Data.Traversable
import System.IO.Unsafe

import Control.Applicative
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
data Chan a = Chan (TChan a)
newChan     :: IO (Chan a)
dupChan     :: Chan a -> IO (Chan a)
writeChan   :: Chan a -> a -> IO ()
tryReadChan :: Chan a -> IO (Maybe a)

newChan               = do
    c' <- atomically $ newTChan
    return (Chan c')

dupChan (Chan c)      = do
    c' <- atomically . dupTChan $ c
    return (Chan c')    

writeChan (Chan c) x  = atomically $ writeTChan c x
tryReadChan (Chan c)  = atomically $ tryReadTChan c


data Event a where
    ENever  :: Event a
    EBoth   :: Event a -> Event a -> Event a

    EPure   :: a -> Event a
    EApply  :: Event (a -> b) -> Event a -> Event b

    EChan   :: Chan a       -> Event a
    ESource :: IO [a]       -> Event a
    ESink   :: (a -> IO b)  -> Event a -> Event b

prepChan :: Chan a -> IO (IO [a])
prepChan ch = do
    ch' <- dupChan ch
    return $ fmap maybeToList $ tryReadChan ch'

prepare :: Event a -> IO (Event a)
prepare (EChan ch)      = do
    ch' <- prepChan ch
    return $ ESource ch' 
prepare (EBoth a b)     = do
    a' <- prepare a
    b' <- prepare b
    return $ EBoth a' b'
prepare (EApply f x)    = do
    f' <- prepare f
    x' <- prepare x
    return $ EApply f' x'
prepare (ESink k a)     = do
    a' <- prepare a
    return $ ESink k a'
prepare x               = return x

run :: Event a -> IO ()
run e = do
    e' <- prepare e
    run' e' >> return ()

runLoop :: Event a -> IO ()
runLoop e = do 
    e' <- prepare e
    runLoop' e'  
    where   
        runLoop' e = run' e >> threadDelay loopInterval >> runLoop' e
        loopInterval = 1000 * 5

run' :: Event a -> IO [a]
run' ENever          = return []
run' (EPure x)       = return [x]
run' (EApply f x)    = run' f <&> run' x
    where
        (<&>) = liftA2 (<*>)
run' (EBoth a b)     = do
    a' <- run' a
    b' <- run' b
    return (a' ++ b')
run' (ESource i)     = i
run' (ESink o x)     = run' x >>= mapM o

single x = [x]

instance Functor (Event) where
    fmap f = (pure f <*>)

instance Applicative (Event) where
    pure  = EPure
    (<*>) = EApply

instance Monoid (Event a) where
    mempty  = ENever
    mappend = EBoth


-- filterE :: (a -> Bool) -> Event a -> Event a
-- filterE p = filterMapE (guard p)
-- 
-- filterMapE :: (a -> Maybe b) -> Event a -> Event b
-- filterMapE p (Event f) = Event $ do
--     f' <- f
--     return $ f' >>= return . list [] (filterMap p)
-- 

-- |
-- The empty event, semantically @[]@.
neverE :: Event a
neverE = mempty

-- |
-- Merged events, semantically @merge xs ys@.
mergeE :: Event a -> Event a -> Event a
mergeE = mappend

-- TODO use different names?

-- |
-- Event reading from external world.
--
-- The computation should be non-blocking.
--
getNonBlockingE :: IO (Maybe a) -> Event a
getNonBlockingE = ESource . fmap maybeToList

-- |
-- Event reading from external world.
--
-- The computation may block and will be run in a separate thread.
--
getE :: IO a -> Event a
getE k = unsafePerformIO $ do
    ch <- newChan
    forkIO $ cycleM $ 
        k >>= writeChan ch
    return (EChan ch)

-- |
-- Event interacting with the external world.
--
-- The computation should be non-blocking.
--
putE :: (a -> IO b) -> Event a -> Event b
putE = ESink

readE :: Chan a -> Event a
readE = EChan

writeE :: Chan a -> Event a -> Event ()
writeE ch = ESink (writeChan ch)

linesIn :: Event String
linesIn = getE getLine 

linesOut :: Event String -> Event ()
linesOut = putE putStrLn


-- 
-- 
-- 
-- 
-- 
-- 
-- -- 
-- -- type MidiSource      = Midi.Source
-- -- type MidiDestination = Midi.Destination
-- -- 
-- -- midiInE :: MidiSource -> Event MidiMessage
-- -- midiInE = undefined
-- -- 
-- -- midiOutE :: MidiDestination -> Event MidiMessage -> Event MidiMessage
-- -- midiOutE = undefined
-- -- 
-- -- type OscMessage = OSC.Message
-- -- 
-- -- oscInE :: Int -> Event OscMessage
-- -- oscInE = undefined
-- -- 
-- -- oscOutE :: String -> Int -> Event OscMessage
-- -- oscOutE = undefined
-- 


guard :: (a -> Bool) -> (a -> Maybe a)
guard p x
    | p x       = Just x
    | otherwise = Nothing

list z f [] = z
list z f xs = f xs

filterMap p = catMaybes . map p   

cycleM x = x >> cycleM x 
