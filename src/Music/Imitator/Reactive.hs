
{-# LANGUAGE GADTs #-}

module Music.Imitator.Reactive (

        -- * Events
        Event,

        -- ** Basic combinators
        neverE,
        mergeE,
        sequenceE,
        -- mergeWithE,

        -- ** Change value of events
        mapE,
        filterE,
        replaceE,
        tickE,
        justE,

        -- ** Accumulated events
        accumE,
        delayE,
        delayE',
        withPrevE,

        -- -- MidiSource,
        -- -- MidiDestination,
        -- -- midiInE,
        -- -- midiOutE,
        -- -- OscMessage,
        -- -- oscInE,
        -- -- oscOutE,
                
        -- * Reactive
        Reactive,

        -- ** Basic combinators
        stepper,
        switcher,
        sample,

        -- ** Accumulated reactives
        accumR,


        -- * Creating events and reactives
        -- ** From standard library
        getLineE,
        putLineE, 

        -- ** From channels
        readChanE,
        writeChanE,

        -- ** From I/O computations
        getE,
        pollE,
        putE,
        -- modifyE,
        
        -- * Run events
        run,
        runLoop,
        runLoopUntil,        

        -- * Utility
        Source,
        Sink,
        newSource,
        newSink,        
        notify,
        showing,

  ) where

import Prelude hiding (mapM)

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Newtype

import Control.Concurrent (forkIO, forkOS, threadDelay)
import System.IO.Unsafe

import Music.Imitator.Reactive.Chan
import Music.Imitator.Reactive.Var

-- import System.MIDI (MidiMessage,  MidiMessage')
-- import qualified System.MIDI            as Midi
-- import qualified Sound.OpenSoundControl as OSC
-- 
-- kPortMidiInfo = unsafePerformIO $ do
--     Midi.initialize
--     num  <- Midi.countDevices
--     infos <- Prelude.mapM Midi.getDeviceInfo [0..num - 1]
--     return infos     



-------------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------------

data Event a where
    ENever  ::                             Event a
    
    EMerge  :: Event a     -> Event a   -> Event a
    ESeq    :: Event a     -> Event b   -> Event b
    EMap    :: (a -> b)    -> Event a   -> Event b
    EPred   :: (a -> Bool) -> Event a   -> Event a

    EChan   :: Chan a                   -> Event a
    ESource :: IO [a]                   -> Event a
    ESink   :: (a -> IO b) -> Event a   -> Event b

    ESamp  :: Reactive a   -> Event b   -> Event a

data Reactive a where
    RConst  :: a                                -> Reactive a

    RStep   :: Var a -> Event a                 -> Reactive a
    RAccum  :: Var a -> Event (a -> a)          -> Reactive a
    
    RApply  :: Reactive (a -> b) -> Reactive a  -> Reactive b
    RJoin   :: Reactive (Reactive a)            -> Reactive a
    

prepE :: Event a -> IO (Event a)
prepE (EMerge a b)     = do
    a' <- prepE a
    b' <- prepE b
    return $ EMerge a' b'
prepE (ESeq a b)     = do
    a' <- prepE a
    b' <- prepE b
    return $ ESeq a' b'
prepE (EMap f x)    = do
    x' <- prepE x
    return $ EMap f x'
prepE (EPred p x)    = do
    x' <- prepE x
    return $ EPred p x'
prepE (ESink k a)     = do
    a' <- prepE a
    return $ ESink k a'
prepE (ESamp r x)    = do
    r' <- prepR r
    x' <- prepE x
    return $ ESamp r' x'
prepE (EChan ch)      = do
    ch' <- prepC ch
    return $ ESource ch' 
prepE x               = return x

prepR :: Reactive a -> IO (Reactive a)
prepR (RStep v x) = do
    x' <- prepE x
    v' <- prepV v
    return $ RStep v' x'
prepR (RAccum v x) = do
    x' <- prepE x
    v' <- prepV v
    return $ RAccum v' x'
prepR (RApply f x) = do
    f' <- prepR f
    x' <- prepR x
    return $ RApply f' x'
prepR (RJoin r) = do
    r' <- prepR r
    return $ RJoin r'
prepR x = return x

prepC :: Chan a -> IO (IO [a])
prepC ch = do
    ch' <- dupChan ch
    return $ fmap maybeToList $ tryReadChan ch'

prepV :: Var a -> IO (Var a)
prepV v = dupVar v

runE :: Event a -> IO [a]
runE ENever          = return []
runE (EMap f x)      = fmap (fmap f) (runE x)
runE (EPred p x)     = fmap (filter p) (runE x)
runE (EMerge a b)    = liftM2 (++) (runE a) (runE b)
-- runE (EMerge a b)    = do
    -- a' <- runE a
    -- b' <- runE b
    -- return (a' ++ b')
runE (ESource i)     = {-putStrLn "Read from source" >>-} i
runE (ESink o x)     = runE x >>= mapM o
runE (ESeq a b)      = runE a >> runE b
runE (ESamp r x)    = do
    r' <- runRS r
    x' <- runE x
    -- return $ fmap (const r') x'
    case x' of
        [] -> return []
        _  -> return [r']

runRS :: Reactive a -> IO a
runRS = fmap last . runR

runR :: Reactive a -> IO [a]
runR (RConst v)      = return [v]
runR (RStep v x)     = do
    v' <- readVar v
    x' <- runE x       
    let ys = (v':x')
    swapVar v (last ys)
    return ys

-- FIXME problem if accumulator is run multiple times
-- Solution: variables must be duplicated just like queues
runR (RAccum v x)   = do
    v' <- readVar v
    x' <- runE x
    let w = (foldr (.) id x') v'
    swapVar v w
    -- putStrLn $ "Number of f in accum: " ++ show (length x')
    return [w]    
runR (RApply f x)   = do
    f' <- runR f
    x' <- runR x
    -- putStrLn $ "Number of f <*> x in apply: " ++ show (length (f' <*> x'))
    return (f' <*> x')
runR (RJoin r)   = do
    r' <- runRS r       -- correct ?
    runR r'


-------------------------------------------------------------------------------------
-- Event API
-------------------------------------------------------------------------------------

instance Functor (Event) where
    fmap    = EMap

instance Monoid (Event a) where
    mempty  = ENever
    mappend = EMerge

-- |
-- Map over occurances, semantically @map p xs@.
mapE :: (a -> b) -> Event a -> Event b
mapE = fmap

-- |
-- Filter occurances, semantically @filter p xs@.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p = EPred p

-- |
-- Replace occurances, semantically @fmap (const x) xs@.
replaceE :: a -> Event a -> Event a
replaceE x = fmap (const x)

justE :: Event (Maybe a) -> Event a
justE = fmap fromJust . filterE isJust

-- |
-- Run both and behave as the second event, sematically @a `seq` b@.
sequenceE :: Event a -> Event b -> Event b
sequenceE = ESeq

-- |
-- The empty event, semantically @[]@.
neverE :: Event a
neverE = mempty

-- |
-- Interleave occurences, semantically @merge xs ys@.
mergeE :: Event a -> Event a -> Event a
mergeE = mappend

-- |
-- Discard values of the event.
tickE :: Event a -> Event ()
tickE = tickME

-- |
-- Discard values, using an arbitrary empty element.
tickME :: Monoid b => Event a -> Event b
tickME = fmap (const mempty)

-- |
-- Event accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumE :: a -> Event (a -> a) -> Event a
a `accumE` e = (a `accumR` e) `sample` e


delayE :: Int -> Event a -> Event a
delayE n = foldr (.) id (replicate n delayE')

delayE' :: Event a -> Event a
delayE' = fmap snd . withPrevE

withPrevE :: Event a -> Event (a, a)
withPrevE e 
    = (joinMaybes' . fmap combineMaybes) 
    $ dup Nothing `accumE` fmap (shift . Just) e
    where      
        shift b (a,_) = (b,a)
        dup x         = (x,x)
        joinMaybes'   = justE
        combineMaybes = uncurry (liftA2 (,))

{-

TODO not sure about these

eventMain :: Event (Maybe Bool) -> IO ()
eventMain = eventMain' . (fmap . fmap) (\r -> if r then ExitSuccess else ExitFailure (-1))

eventMain' :: Event (Maybe ExitCode) -> IO ()
eventMain' e = do
    code <- runLoopUntil e
    exitWith code        
-}

-------------------------------------------------------------------------------------
-- Reactive API
-------------------------------------------------------------------------------------

instance Monoid a => Monoid (Reactive a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Functor Reactive where
    fmap f = (pure f <*>)

instance Applicative Reactive where
    pure x = stepper x neverE 
    (<*>) = RApply

instance Monad Reactive where
    return  = pure
    x >>= k = (RJoin . fmap k) x

-- |
-- Step between values.
--
stepper  :: a -> Event a -> Reactive a
stepper x e = RStep (newVar x) e

-- |
-- Switch between reactives.
--
switcher :: Reactive a -> Event (Reactive a) -> Reactive a
switcher r e = join (stepper r e)

sample :: Reactive b -> Event a -> Event b
sample = ESamp

-- |
-- Reactive accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumR :: a -> Event (a -> a) -> Reactive a
accumR x = RAccum (newVar x)

-- |
-- Count occurances.
--
count :: Event a -> Reactive Int
count = accumR 0 . fmap (const succ)

-- diffE :: Event a -> Event a



-- turnOn  :: Event a -> Reactive Bool
-- turnOff :: Event a -> Reactive Bool
-- toggle  :: Event a -> Reactive Bool



{-

sample   :: Event a -> Reactive b -> Event b
joinR    :: Reactive (Reactive a) -> Reactive a
modify   :: Event (a -> a) -> Reactive a -> Reactive a
set      :: Event a        -> Reactive a -> Reactive a
-}

       
-------------------------------------------------------------------------------------
-- Lifting IO etc
-------------------------------------------------------------------------------------

-- |
-- Event reading from external world.
-- The computation should be blocking and is polled exactly once per occurence.
--
-- This function can be used with standard I/O functions.
--
getE :: IO a -> Event a
getE k = unsafePerformIO $ do
    ch <- newChan
    forkIO $ cycleM $ 
        k >>= writeChan ch
    return (EChan ch)

-- |
-- Event reading from external world.
-- The computation should be non-blocking and may be polled repeatedly for each occurence.
--
-- This function should be used with /non-effectful/ functions, typically functions that
-- observe the current value of some external property.
-- You should /not/ use this function with standard I/O functions as this
-- may lead to non-deterministic reads (i.e. loss of data).
--
pollE :: IO (Maybe a) -> Event a
pollE = ESource . fmap maybeToList

-- Event interacting with the external world.
-- The computation should be non-blocking and its values will be contested.
--
-- modifyE :: (a -> IO b) -> Event a -> Event b
-- modifyE = ESink

-- |
-- Event writing to the external world.
--
-- This function can be used with standard I/O functions.
--
putE :: (a -> IO ()) -> Event a -> Event a
putE k = ESink $ \x -> do
    k x
    return x

readChanE :: Chan a -> Event a
readChanE = EChan

writeChanE :: Chan a -> Event a -> Event a
writeChanE ch e = ESink (writeChan ch) e `sequenceE` e

getLineE :: Event String
getLineE = getE getLine 

putLineE :: Event String -> Event String
putLineE = putE putStrLn


-------------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------------

-- | 
-- Run the given event once.
--
run :: Event a -> IO ()
run e = do
    f <- prepE e
    runE f
    return ()

-- | 
-- Run the given event for ever.
--
runLoop :: Event a -> IO ()
runLoop e = do 
    f <- prepE e
    runLoop' f  
    where   
        runLoop' g = do
            runE g
            threadDelay kLoopInterval >> runLoop' g

-- | 
-- Run the given event until the first @Just x@  occurence, then return @x@.
--
runLoopUntil :: Event (Maybe a) -> IO a
runLoopUntil e = do 
    f <- prepE e
    runLoopUntil' f  
    where   
        runLoopUntil' g = do
            r <- runE g
            case (catMaybes r) of 
                []    -> threadDelay kLoopInterval >> runLoopUntil' g
                (a:_) -> return a

kLoopInterval = 1000 * 1



-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

type Source a = Event a
type Sink a   = Event a -> Event a

notify :: String -> Event a -> Event a
notify m x = putLineE (fmap (const m) x) `sequenceE` x

showing :: Show a => String -> Event a -> Event a
showing m x = putLineE (fmap (\x -> m ++ show x) x) `sequenceE` x

newSource :: IO (a -> IO (), Source a)
newSource = do
    ch <- newChan
    return (writeChan ch, readChanE ch)

newSink :: IO (IO (Maybe a), Sink a)
newSink = do
    ch <- newChan
    return (tryReadChan ch, writeChanE ch)  


-------------------------------------------------------------------------------------

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


guard :: (a -> Bool) -> (a -> Maybe a)
guard p x
    | p x       = Just x
    | otherwise = Nothing

list z f [] = z
list z f xs = f xs

filterMap p = catMaybes . map p   

cycleM x = x >> cycleM x 

single x = [x]

-- | Pass through @Just@ occurrences.
joinMaybes :: MonadPlus m => m (Maybe a) -> m a
joinMaybes = (>>= maybe mzero return)

-- | Pass through values satisfying @p@.
filterMP :: MonadPlus m => (a -> Bool) -> m a -> m a
filterMP p m = joinMaybes (liftM f m)
 where
   f a | p a        = Just a
       | otherwise  = Nothing
