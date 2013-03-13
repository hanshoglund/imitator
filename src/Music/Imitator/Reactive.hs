
module Music.Imitator.Reactive where

import Data.Monoid
import Data.Traversable
-- import Control.Concurrent.Chan

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan

-- Factor out channels
type Chan a = TChan a
newChan     :: IO (Chan a)
dupChan     :: Chan a -> IO (Chan a)
writeChan   :: Chan a -> a -> IO ()
readChan    :: Chan a -> IO a
peekChan    :: Chan a -> IO a
tryReadChan :: Chan a -> IO (Maybe a)
tryPeekChan :: Chan a -> IO (Maybe a)
newChan       = atomically $ newTChan
dupChan       = atomically . dupTChan
writeChan c   = atomically . writeTChan c
readChan      = atomically . readTChan
peekChan      = atomically . peekTChan
tryReadChan   = atomically . tryReadTChan
tryPeekChan   = atomically . tryPeekTChan


newtype Event a = Event { getEvent :: IO (Maybe a) }

instance Functor Event where
    fmap f (Event g) = Event $ do
        x <- g
        return $ fmap f x

instance Monad Event where
    return = Event . return . return
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
            _         -> do
                y <- g
                case y of
                    (Just y) -> return $ Just y
                    _         -> return $ Nothing

-- -- wrapIO    :: (a -> IO b)  -> Event a -> Event b
-- -- wrapIOGet :: (IO a)       -> Event a
-- -- wrapIOSet :: (a -> IO ()) -> Event a -> Event ()

readE :: Chan a -> Event a
readE ch = Event $ do
    x <- tryReadChan ch
    case x of
        (Just x) -> return (return x)
        Nothing  -> return Nothing

writeE :: Chan a -> Event a -> Event a
writeE ch (Event f) = Event $ do
    x <- f
    case x of
        (Just x)  -> writeChan ch x
        _         -> return ()
    return x

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
run :: Event a -> IO ()
run (Event f) = do
    x <- f
    return ()

runLoop :: Event a -> IO ()
runLoop e = run e >> runLoop e  


