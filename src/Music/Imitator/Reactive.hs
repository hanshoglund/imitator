
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
        readE,
        writeE,
        readIOE,
        writeIOE,
        run,
        runLoop
  ) where

import Data.Monoid
import Data.Traversable

import Control.Newtype
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan


import qualified Sound.PortMidi         as Midi
import qualified Sound.OpenSoundControl as OSC



-- Factor out channels
newtype Chan a = Chan { getChan :: TChan a}
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


newtype Event a = Event { getEvent :: IO (Maybe a) }

-- instance Newtype (Event a) (IO (Maybe a)) where
--     pack    = Event
--     unpack  = getEvent

instance Functor Event where
    fmap f = Event . (fmap (fmap f)) . getEvent

instance Monad Event where
    return  = Event . return . return
    (Event f) >>= k = Event $ do
        x <- f
        case x of
            (Just x) -> (getEvent . k) x
            Nothing  -> return Nothing

instance Monoid (Event a) where
    mempty = Event $ return $ Nothing
    Event f `mappend` Event g = Event $ do
        x <- f
        case x of
            (Just x) -> return $ Just x
            _        -> do
                y <- g
                case y of
                    (Just y) -> return  $ Just y
                    _        -> return $ Nothing


readIOE :: IO (Maybe a) -> Event a
readIOE = Event

writeIOE :: (a -> IO ()) -> Event a -> Event a
writeIOE g (Event f) = Event $ do
    x <- f
    case x of
        (Just x)  -> g x
        _         -> return ()
    return x


readE :: Chan a -> Event a
readE ch = readIOE (tryReadChan ch)

writeE :: Chan a -> Event a -> Event a
writeE ch e = writeIOE (writeChan ch) e

-- -- TODO make non-blocking    
-- linesIn  :: Event String
-- linesIn = Event $ fmap EventVal getLine
-- 
-- linesOut :: Event String -> Event String
-- linesOut (Event f) = Event $ do
--     x <- f
--     case x of
--         (EventVal x) -> putStrLn x
--         _         -> return ()
--     return x
-- 

-- |
-- Run an event.                                     
-- 
-- This may result in wrapped actions being executed. 
-- If more than one event refer to a single channel they compete for its contents (i.e. non-determinism).
--
run :: Event a -> IO ()
run (Event f) = do
    x <- f
    return ()

runLoop :: Event a -> IO ()
runLoop e = run e >> runLoop e  


