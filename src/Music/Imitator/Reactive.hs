
{-# LANGUAGE GADTs #-}

module Music.Imitator.Reactive (

        -- * Events
        Event,

        -- ** Basic combinators
        neverE,
        mergeE,
        eitherE,
        sequenceE,

        -- ** Change value of events
        mapE,
        filterE,
        retainE,
        partitionE,
        splitE,
        concatE,
        replaceE,
        tickE,
        justE,

        -- ** Accumulated events
        prevE,
        delayE,
        bufferE,
        withPrevE,
        accumE,
        foldpE,
        scanlE,
        monoidE,
        -- liftMonoidE,
        sumE,
        productE,
        allE,
        anyE,
        countE,

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
        activate,
        apply,
        sample,
        sampleWith,
        bothR,
        countR,
        gate,
        onR,
        offR,
        toggleR,

        -- ** Accumulated reactives
        accumR,
        mapAccum,

        -- * Creating events and reactives
        -- ** From standard library
        getCharE,
        putCharE,
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
import Data.Either
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
    EConcat ::                Event [a] -> Event a

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
prepE (EConcat x)    = do
    x' <- prepE x
    return $ EConcat x'
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
runE (EConcat x)     = fmap concat (runE x)
runE (EMerge a b)    = liftM2 (++) (runE a) (runE b)
runE (ESource i)     = i
runE (ESink o x)     = runE x >>= mapM o
runE (ESeq a b)      = runE a >> runE b
runE (ESamp r x)    = do
    r' <- runRS r
    x' <- runE x
    return $ fmap (const r') x'
    -- case x' of
        -- [] -> return []
        -- _  -> return [r']

runRS :: Reactive a -> IO a
runRS = fmap last . runR
-- Note: last is safe as reactives (per definition) always have at least one value

runR :: Reactive a -> IO [a]
runR (RConst v)      = return [v]
runR (RStep v x)     = do
    v' <- readVar v
    x' <- runE x       
    let ys = (v':x')
    swapVar v (last ys)
    return ys
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
-- The empty event, semantically @[]@.
--
neverE :: Event a
neverE = mempty

-- |
-- Interleave values, semantically @merge xs ys@.
--
mergeE :: Event a -> Event a -> Event a
mergeE = mappend

-- |
-- Interleave values of different types. See also 'bothR'.
--
eitherE :: Event a -> Event b -> Event (Either a b)
a `eitherE` b = fmap Left a `mergeE` fmap Right b

-- |
-- Run both and behave as the second event, sematically @a `seq` b@.
--
sequenceE :: Event a -> Event b -> Event b
sequenceE = ESeq

-- |
-- Map over values, semantically @f \<$> xs@.
mapE :: (a -> b) -> Event a -> Event b
mapE = (<$>)

-- |
-- Filter values, semantically @filter p xs@.
--
filterE :: (a -> Bool) -> Event a -> Event a
filterE p = EPred p

-- |
-- Retain values, semantically @retain p xs@.
--
retainE :: (a -> Bool) -> Event a -> Event a
retainE p = EPred (not . p)

-- |
-- 
--
concatE :: Event [a] -> Event a
concatE = EConcat

-- |
-- Partition values, semantically @partition p xs@.
-- 
-- > mergeE x y  where (x, y) = partitionE p e  ===  e      for all p
--
partitionE :: (a -> Bool) -> Event a -> (Event a, Event a)
partitionE p e = (filterE p e, retainE p e)

-- | 
-- Partition values of different types. See also 'partitionE'.
--
-- > eitherE x y where (x, y) = splitE e        ===  e
-- 
splitE :: Event (Either a b) -> (Event a, Event b)
splitE e = (justE $ fromLeft <$> e, justE $ fromRight <$> e)

-- |
-- Discard empty values.
--
justE :: Event (Maybe a) -> Event a
justE = fmap fromJust . filterE isJust

-- |
-- Replace values, semantically @x <$ e@.
--
replaceE :: b -> Event a -> Event b
replaceE x = (x <$)

-- |
-- Discard values of the event.
--
tickE :: Event a -> Event ()
tickE = replaceE ()

-- |
-- Discard values, using an arbitrary empty element.
--
tickME :: Monoid b => Event a -> Event b
tickME = replaceE mempty

-- |
-- Event accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumE :: a -> Event (a -> a) -> Event a
a `accumE` e = (a `accumR` e) `sample` e

-- |
-- Create a past-dependent event.
--
-- > scanlE f z x = foldpE (flip f) f z x
--        
foldpE :: (a -> b -> b) -> b -> Event a -> Event b
foldpE f a e = a `accumE` (f <$> e)

-- |
-- Create a past-dependent event. This combinator corresponds to 'scanl' on streams.
--
-- > scanlE f z x = foldpE (flip f) f z x
--        
scanlE :: (a -> b -> a) -> a -> Event b -> Event a
scanlE f = foldpE (flip f)

        
-- |
-- Create a past-dependent event using a 'Monoid' instance.
--
monoidE :: Monoid a => Event a -> Event a
monoidE = foldpE mappend mempty

liftMonoidE :: Monoid m => (a -> m) -> (m -> a) -> Event a -> Event a
liftMonoidE i o = fmap o . monoidE . fmap i

sumE :: Num a => Event a -> Event a
sumE = liftMonoidE Sum getSum

productE :: Num a => Event a -> Event a
productE = liftMonoidE Product getProduct

allE :: Event Bool -> Event Bool
allE = liftMonoidE All getAll

anyE :: Event Bool -> Event Bool
anyE = liftMonoidE Any getAny


-- |
-- Count values.
--
countE :: Enum b => Event a -> Event b
countE = accumE (toEnum 0) . fmap (const succ)

-- |
-- Delay by a single value.
--
prevE :: Event a -> Event a
prevE = fmap snd . withPrevE

-- |
-- Delay by @n@ values.
--
delayE :: Int -> Event a -> Event a
delayE n = foldr (.) id (replicate n prevE)

bufferE :: Int -> Event a -> Event [a]
bufferE n = foldpE (\x xs -> x : take (n-1) xs) []

--Ord t => (t, a) -> Event t -> Event [a]

-- |
-- Pack with previous value.
--
withPrevE :: Event a -> Event (a, a)
withPrevE = justE . fmap k . bufferE 2
    where
        k []      = Nothing
        k [x]     = Nothing
        k (a:b:_) = Just (a,b)

-- withPrevE e 
--     = (joinMaybes' . fmap combineMaybes) 
--     $ dup Nothing `accumE` fmap (shift . Just) e
--     where      
--         shift b (a,_) = (b,a)
--         dup x         = (x,x)
--         joinMaybes'   = justE
--         combineMaybes = uncurry (liftA2 (,))

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

constR :: a -> Reactive a
constR = pure

-- |
-- Step between values.
--
stepper  :: a -> Event a -> Reactive a
stepper x e = RStep (newVar x) e

-- |
-- Step between values without initial.
--
activate :: Event a -> Reactive (Maybe a)
activate e = Nothing `stepper` fmap Just e

-- |
-- Switch between reactives.
--
switcher :: Reactive a -> Event (Reactive a) -> Reactive a
r `switcher` e = join (r `stepper` e)

-- | 
-- Apply the values of an event to a time-varying function.
--
-- > r `apply` e = uncurry ($) <$> (r `sampleWith` e)
-- 
apply :: Reactive (a -> b) -> Event a -> Event b
apply r e = (uncurry ($)) <$> sampleWith r e

-- |
-- Sample value of reactive.
--
-- > r `sample` e = (const <$> r) `apply` e
--
sample :: Reactive a -> Event b -> Event a
sample = ESamp

-- |
-- Sample value of reactive with the value of the trigger.
--
-- > r `sampleWith` e = ((,) <$> r) `apply` e
--
sampleWith :: Reactive a -> Event b -> Event (a, b)
sampleWith r e = sample (liftA2 (,) r (eventToReactive e)) e
    where
        eventToReactive = stepper (error "Partial reactive")

-- |
-- Combine reactives. See also 'eitherE'.
--
bothR :: Reactive a -> Reactive b -> Reactive (a, b)
bothR = liftA2 (,)

-- |
-- Reactive accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumR :: a -> Event (a -> a) -> Reactive a
accumR x = RAccum (newVar x)

-- | 
-- Efficient combination of 'accumE' and 'accumR'.
-- 
mapAccum :: a -> Event (a -> (b,a)) -> (Event b, Reactive a)
mapAccum acc ef = (fst <$> e, stepper acc (snd <$> e))
    where 
        e = accumE (undefined,acc) ((. snd) <$> ef)

-- |
-- Count values.
--
countR :: Enum b => Event a -> Reactive b
countR = accumR (toEnum 0) . fmap (const succ)

-- diffE :: Event a -> Event a

gate :: Reactive Bool -> Event a -> Event a
gate r e = justE $ apply (fmap (\b x -> if b then Just x else Nothing) r) e

onR :: Event a -> Reactive Bool
onR = fmap isJust . activate

offR :: Event a -> Reactive Bool
offR = fmap not . onR

toggleR :: Event a -> Reactive Bool
toggleR = fmap odd . countR


{-

modify   :: Event (a -> a) -> Reactive a -> Reactive a
set      :: Event a        -> Reactive b -> Reactive a
-}

       
-------------------------------------------------------------------------------------
-- Lifting IO etc
-------------------------------------------------------------------------------------

-- |
-- Event reading from external world.
-- The computation should be blocking and is polled exactly once per value.
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
-- The computation should be non-blocking and may be polled repeatedly for each value.
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

-- |
-- Event reading from a channel.
--
readChanE :: Chan a -> Event a
readChanE = EChan

-- |
-- Event writing to a channel.
--
writeChanE :: Chan a -> Event a -> Event a
writeChanE ch e = ESink (writeChan ch) e `sequenceE` e

-- |
-- Event version of 'getChar'.
--
getCharE :: Event Char
getCharE = getE getChar 

-- |
-- Event version of 'putChar'.
--
putCharE :: Event Char -> Event Char
putCharE = putE putChar

-- |
-- Event version of 'getLine'.
--
getLineE :: Event String
getLineE = getE getLine 

-- |
-- Event version of 'putStrLn'.
--
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
-- Run the given event until the first @Just x@  value, then return @x@.
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

kLoopInterval = 1000 * 5



-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

type Source a = Event a
type Sink a   = Event a -> Event a

-- |
-- Behaves like the original event but writes a given message to the standard
-- output for each value.
-- 
notify :: String -> Event a -> Event a
notify m x = putLineE (fmap (const m) x) `sequenceE` x

-- |
-- Behaves like the original event but writes its value, prepended by the given
-- message, for each value.
-- 
showing :: Show a => String -> Event a -> Event a
showing m x = putLineE (fmap (\x -> m ++ show x) x) `sequenceE` x

-- |
-- Creates a new source and a computation that writes  it.
-- 
newSource :: IO (a -> IO (), Source a)
newSource = do
    ch <- newChan
    return (writeChan ch, readChanE ch)

-- |
-- Creates a new sink and a computation that reads from it.
-- 
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

fromLeft  (Left  a) = Just a
fromLeft  (Right b) = Nothing
fromRight (Left  a) = Nothing
fromRight (Right b) = Just b                         


