
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : GPL
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : stable
-- Portability : portable
--
-- Wrapper around hsc3 for the purpose of Imitator.
--
-------------------------------------------------------------------------------------

module Music.Imitator.Sound (

        module Music.Imitator.Sound.Prim,
        
        -- * Generators
        
        -- ** Oscillators
        sine,
        saw,
        phasor,
        phasor',
        pulse',
        impulse,

        noise,     
        grayNoise,
        pinkNoise,
        brownNoise,
        clipNoise,
        dust,
        dust2,
        -- lfCub,
        -- lcGauss,
        -- lfPar,
        -- lfPulse,
        -- lfSaw,
        -- lfTri,
        
        -- ** Envelopes
        env,
        envSust,
        envLoop,
        envGen,
        envGen',

        -- ** Filters
        -- bpf,
        -- combN,
        -- combL,
        -- combC,
        -- decay,
        -- gate,
        -- timer,

        -- ** Delay
        -- decay2,
        -- delay1,
        -- delay2,
        -- delayN,
        -- delayC,
        -- delayL,

        -- ** Reverb
        -- freeVerb,

        -- ** Spacialization
        -- *** Encoders
        foaPanB,
        foaOmni,

        -- *** Decoders
        decode,       

        -- *** Transformations
        foaRotate,
        foaTilt,
        foaTumble,

        -- ** Mixing
        select,
        select',

        -- ** Multichannel
        (!),
        U.mce,
        U.mce2,
        U.mce3,
        U.mceChannels,
        getChannel,
        numChannels,

        -- ** Control
        control,
        mouse,
        mouseButton,

        -- ** Buffers
        recordBuf,
        playBuf,
        grainBuf,   

        -- ** Buses (I/O)
        input,
        output,
        feedback,


        -- * Server
        -- ** Buffers
        newBuffer,
        readBuffer,
        readBuffer',
        freeBuffer,

        -- -- ** Synthdefs
        -- receive,
        -- loadDir,
        
        -- ** Testing
        play,
        play',
        abort,
        sendStd,      
        
        -- ** Server control
        -- ** Non real-time
        NRT,
        runServer,

        -- ** Real-time
        -- *** Start and stop
        startServer,
        stopServer,
        
        -- *** Server status
        isServerRunning,
        serverStatusData,
        serverStatusDataR,
        serverStatus,
        serverUgens,
        serverSynths,
        serverGroups,
        serverInstruments,
        serverCPUAverage,
        serverCPUPeak,
        serverSampleRateNominal,
        serverSampleRateActual,

        printServerStatus,
        
        -- * Constants
        kStdServer,
        kStdPort,
        kMasterVolume,
        kInputBuses,
        kOutputBuses,
        kAudioBuses,
        kControlBuses,
        kOutputOffset,
        kAudioBusOffset,
        kControlBusOffset,
        kNumSpeakers,     
        kSampleRate,
  ) where

import Data.Monoid
import Data.Functor.Apply
import Control.Reactive
import Control.Exception ( try, SomeException )
import Control.Applicative
import Control.Concurrent ( threadDelay )
import System.Directory ( getCurrentDirectory )

import Data.Bits
import System.Random
import System.IO.Unsafe ( unsafePerformIO )

import qualified Sound.SC3.UGen             as U
import qualified Sound.SC3.UGen.Noise.Monad as N
import qualified Sound.SC3.Server.FD        as S

import Sound.OSC.Transport.FD.UDP ( openUDP )
import Sound.OpenSoundControl.Type ( Message(..), Datum(..), Bundle(..) )
import Sound.SC3.Server.NRT ( NRT(..), writeNRT )
import Sound.OSC.Transport.FD.UDP ( UDP )

import Music.Imitator.Sound.Prim
import Music.Imitator.Util




-- |
-- Sinusoid generator.
--
-- > sine freq                 
--
sine :: UGen -> UGen
sine freq  = U.sinOsc AR freq 0

-- |
-- Impulse generator.
--
-- > impulse freq                 
--
impulse :: UGen -> UGen
impulse freq = U.impulse AR freq 0


-- |
-- Phase generator.
--
-- Loops 0-1, jumps to 0 on trigger.
--
-- > phasor trig freq                 
--
phasor :: UGen -> UGen -> UGen
phasor trig freq = phasor' trig (freq/kSampleRate) 0 1 0

-- |
-- Phase generator.
--
-- > phasor' trig diff start end resetPos                 
--
phasor' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor' trig rate start end resetPos = U.phasor AR trig rate start end resetPos

-- |
-- Pulse generator.
--
-- > pulse freq width                 
--
pulse' :: UGen -> UGen -> UGen
pulse' freq width = U.pulse AR freq width

-- |
-- Sawtooth wave generator.
--
-- > saw freq                 
--
saw :: UGen -> UGen
saw freq = U.saw AR freq

-- |
-- Vibrato generator.
--
-- > vibrato freq rate depth delay onset rateVar depthVar initPhase                 
--
vibrato :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
vibrato freq rate depth delay onset rateVar depthVar initPhase = 
    U.vibrato AR freq rate depth delay onset rateVar depthVar initPhase
    
-- |
-- Noise generator.
--
-- > noise                 
--
noise :: UGen
noise = unsafePerformIO $ N.whiteNoise AR

-- |
-- Noise generator.
--
-- > grayNoise                 
--
grayNoise :: UGen
grayNoise = unsafePerformIO $ N.grayNoise AR

-- |
-- Noise generator.
--
-- > pinkNoise                 
--
pinkNoise :: UGen
pinkNoise = unsafePerformIO $ N.pinkNoise AR

-- |
-- Noise generator.
--
-- > brownNoise                 
--
brownNoise :: UGen
brownNoise = unsafePerformIO $ N.brownNoise AR

-- |
-- Noise generator.
--
-- > clipNoise                 
--
clipNoise :: UGen
clipNoise = unsafePerformIO $ N.clipNoise AR

-- |
-- Random impulses in (0,1).
--
-- > dust density                 
--
dust :: UGen -> UGen
dust density = unsafePerformIO $ N.dust AR density

-- |
-- Random impulses in (-1,1).
--
-- > dust2 density
--
dust2 :: UGen -> UGen
dust2 density = unsafePerformIO $ N.dust2 AR density



-- |
-- Control input.
--
-- > control name default
--
control :: String -> Double -> UGen
control = U.control AR

-- |
-- Mouse position generator.
--
-- > let (x, y) = mouse
--
mouse :: (UGen, UGen)
mouse    = (U.mouseX KR 0 1 Linear 0.2, U.mouseY KR 0 1 Linear 0.2)


-- |
-- Mouse button generator.
--
-- > mouseButton
--
mouseButton :: UGen
mouseButton = U.mouseButton KR 0 1 0.2


-- |
-- Record to buffer.
--
-- > recordBuf buffer offset trig onOff input 
--
recordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
recordBuf buffer offset trig onOff input 
    = U.recordBuf AR buffer offset 1 0 onOff NoLoop trig RemoveSynth input

--------------------------------------------------------------------------------
-- Envelopes
--------------------------------------------------------------------------------

-- |
-- Create a fixed-time envelope.
--
-- > env val1 [(dur2,val2,curve2)..] 
--
env :: UGen -> [(UGen, UGen, U.EnvCurve)] -> U.Envelope UGen
env _ [] = error "env: Empty envelope"
env z as = U.Envelope (z:levels) times curves Nothing Nothing
    where
        (times, levels, curves) = unzip3 as


-- |
-- Create a sustained envelope.
--
-- > envSust val1 [(dur2,val2,curve2)..] [(dur2,val2,curve2)..]
--
envSust :: UGen -> [(UGen, UGen, U.EnvCurve)] -> [(UGen, UGen, U.EnvCurve)] -> U.Envelope UGen 
envSust _ [] [] = error "envSust: Empty envelope"
envSust z as bs = U.Envelope (z:levels) times curves (Just $ length as) Nothing
    where
        (times, levels, curves) = unzip3 $ as ++ bs

-- |
-- Create a looped envelope.
--
-- > envSust val2 [(dur2,val2,curve2)..] [(dur2,val2,curve2)..] [(dur2,val2,curve2)..]
--
envLoop :: UGen -> [(UGen, UGen, U.EnvCurve)] -> [(UGen, UGen, U.EnvCurve)] -> [(UGen, UGen, U.EnvCurve)] -> U.Envelope UGen
envLoop _ [] [] [] = error "envLoop: Empty envelope"
envLoop z as bs cs = U.Envelope (z:levels) times curves (Just $ length as) (Just $ length as + length bs)
    where
        (times, levels, curves) = unzip3 $ as ++ bs ++ cs


-- |
-- Create a generator from an envelope.
--
-- > envGen trigger envelope
--
envGen :: UGen -> U.Envelope UGen -> UGen
envGen t e = envGen' 1 t e

-- |
-- Create a generator from an envelope.
--
-- > envGen' timeScale trigger envelope
--
envGen' :: UGen -> UGen -> U.Envelope UGen -> UGen
envGen' ts t e = U.envGen AR t 1 0 ts RemoveSynth e



--------------------------------------------------------------------------------
-- Buffers
--------------------------------------------------------------------------------

-- |
-- Play from buffer.
--
-- > playBuf numChan bufNum trig speed startPos 
--
playBuf :: Int -> UGen -> UGen -> UGen -> UGen -> UGen
playBuf numChan bufNum trig speed startPos 
    = U.playBuf numChan AR bufNum speed trig startPos NoLoop RemoveSynth

-- |
-- Play grains from buffer.
--
-- > grainBuf numChan bufNum trig speed centerPos dur
--
grainBuf :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
grainBuf numChan bufNum trig speed centerPos dur 
    = U.tGrains numChan trig bufNum speed centerPos dur 0 1 noInterp
    where
        (noInterp, linInterp, cubInterp) = (1,2,4)


--------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------

-- |
-- Read input.
--
-- > input numCh bus
--
input :: Int -> UGen -> UGen
input numCh bus = U.in' numCh U.AR bus


-- |
-- Read input.
--
-- > output bus input
--
output :: UGen -> UGen -> UGen
output bus = U.out bus

-- |
-- Read input without erasing it.
--
-- > feedback numCh bus
--
feedback :: Int -> UGen -> UGen
feedback numCh bus = U.inFeedback numCh bus


--------------------------------------------------------------------------------
-- Spacialization
--------------------------------------------------------------------------------

-- |
-- > foaOmni input
--
foaOmni :: UGen -> UGen
foaOmni input = U.mce $ replicate 4 input

-- |
-- > foaPanB azimuth elev input
--
foaPanB :: UGen -> UGen -> UGen -> UGen
foaPanB azimuth elev input = U.mkFilter "FoaPanB" [input, azimuth, elev] 4

-- |
-- > decode numSpeakers input
--
decode :: Int -> UGen -> UGen
decode numSpeakers input = U.decodeB2 numSpeakers w x y 0
    where
        [w,x,y,_] = U.mceChannels input


-- |
-- > foaRotate angle input
--
foaRotate :: UGen -> UGen -> UGen
foaRotate angle input = U.mkFilter "FoaRotate" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = U.mceChannels input

foaTilt :: UGen -> UGen -> UGen
foaTilt angle input = U.mkFilter "FoaTilt" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = U.mceChannels input

foaTumble :: UGen -> UGen -> UGen
foaTumble angle input = U.mkFilter "FoaTumble" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = U.mceChannels input


--------------------------------------------------------------------------------
-- Mixing
--------------------------------------------------------------------------------

-- |
-- Crossfade between list elements.
--
-- > select n as  =  as !! 0
--
select :: UGen -> [UGen] -> UGen
select n []     = 0
select n (a:as) = select' (limit 0 1 n) a (select (n-1) as)
    where
        limit m n x = m `max` (n `min` x)

-- select 0 a b = a
-- select 1 a b = b
select' :: UGen -> UGen -> UGen -> UGen
select' n a b = f n a + g n b
    where
        f n x = cos (n*(pi/2)) * x
        g n x = sin (n*(pi/2)) * x   
        
--------------------------------------------------------------------------------
-- Multichannel
--------------------------------------------------------------------------------        

-- |
-- Multichannel duplication.
-- 
(!) :: UGen -> Int -> UGen
g ! n = U.mce $ concat $ replicate n (U.mceChannels g)

-- |
-- Multichannel extraction.
-- 
getChannel :: Int -> UGen -> UGen
getChannel n = (!! n) . U.mceChannels

-- |
-- Count number of channels.
-- 
numChannels :: UGen -> Int
numChannels = length . U.mceChannels



--------------------------------------------------------------------------------
-- Buffer allocation
--------------------------------------------------------------------------------

-- |
-- Allocate a new buffer
-- 
-- > newBuffer bufNum frames channel
--
newBuffer :: Int -> Int -> Int -> Message
newBuffer = S.b_alloc

-- |
-- Read buffer from a file.
-- 
-- > readBuffer bufNum filePath
--
readBuffer :: Int -> FilePath -> Message
readBuffer num path = readBuffer' num path Nothing Nothing Nothing

-- |
-- Read buffer from a file.
-- 
-- > readBuffer bufNum filePath fileStart fileNumFrames bufStart
--
readBuffer' :: Int -> String -> Maybe Int -> Maybe Int -> Maybe Int -> Message
readBuffer' bufNum filePath fileStart numFrames bufStart 
    = S.b_read 
        bufNum 
        filePath 
        (maybe 0    id fileStart) 
        (maybe (-1) id numFrames) 
        (maybe 0    id bufStart)  
        False

-- |
-- Free a buffer.
-- 
-- > free bufNum
--
freeBuffer :: Int -> Message
freeBuffer = S.b_free






--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- |
-- Add a generator to the synthesis graph, wrapping it in standard output and gain.
--
play :: UGen -> IO ()
play gen = do
    asyncStd $ recvMsg
    sendStd  $ addToTailMsg
        where           
            recvMsg         = S.d_recv $ S.synthdef "playGen" (U.out kOutputOffset $ gen * kMasterVolume)
            addToTailMsg    = S.s_new "playGen" 111 S.AddToTail 0 []

-- |
-- Add a generator to the synthesis graph without wrapping. Use this function carefully.
--
play' :: UGen -> IO ()
play' gen = do
    asyncStd $ recvMsg
    sendStd  $ addToTailMsg
        where           
            recvMsg         = S.d_recv $ S.synthdef "playGen'" gen
            addToTailMsg    = S.s_new "playGen" 111 S.AddToTail 0 []

-- |
-- Abort all current synthesis.
--
abort :: IO ()
abort = do
    sendStd $ S.g_deepFree [0]

-- |
-- Send a message to the server over the standard connection.
--
sendStd :: Message -> IO ()
sendStd msg = do
    checkServerActive
    fd <- kStdServer
    S.send fd msg

asyncStd :: Message -> IO Message
asyncStd msg = do
    checkServerActive
    fd <- kStdServer
    S.async fd msg

-- ./scsynth -N Cmds.osc _ NRTout.aiff 44100 AIFF int16
-- |
-- Run a non-realtime server.
--
runServer :: NRT -> Maybe FilePath -> Maybe FilePath -> IO ()
runServer cmds input output = do
    cd <- getCurrentDirectory
    writeNRT (cd ++ "score.osc") cmds
    execute "scsynth" [
        "-v",   "1",
        "-N",   (cd ++ "score.osc"), 
                maybe "_" id $ input, 
                maybe "_" id $ output, 
                show kStdOutputSampleRate, 
                kStdOutputType, 
                kStdOutputFormat,
        
        "-i",   show kInputBuses,
        "-o",   show kOutputBuses,
        "-a",   show kAudioBuses,
        "-c",   show kControlBuses
        ]



-- |
-- Start the server.
--
startServer :: IO ()
startServer = do
    execute "scsynth" [
        "-v",   "1", 
        "-u",   show kStdPort,
        "-S",   show kStdSampleRate,
        "-m",   show kRealTimeMemorySize,

        "-i",   show kInputBuses,
        "-o",   show kOutputBuses,
        "-a",   show kAudioBuses,
        "-c",   show kControlBuses
        ]


-- |
-- Stop the server.
--
stopServer :: IO ()
stopServer = execute "killall" ["scsynth"]

checkServerActive :: IO ()
checkServerActive = do
    active <- isServerRunning
    case (active) of
        False -> fail "Server not running"
        True  -> return ()

-- |
-- Return whether the server is active.
--
isServerRunning :: IO Bool
isServerRunning = do
    status <- try serverStatusData :: IO (Either SomeException [Datum])
    case status of
        Left _  -> return False
        Right _ -> return True

-- |
-- Get server status.
--
serverStatus :: IO [String]
serverStatus = S.withSC3 S.serverStatus     

-- |
-- Get server status as a raw message.
--
serverStatusData :: IO [Datum]
serverStatusData = S.withSC3 S.serverStatusData

serverStatusDataR :: Reactive [Datum]
serverStatusDataR = pollR (fmap Just $ serverStatusData)



serverUgens                 :: Reactive Int
serverSynths                :: Reactive Int
serverGroups                :: Reactive Int
serverInstruments           :: Reactive Int
serverCPUAverage            :: Reactive Double
serverCPUPeak               :: Reactive Double
serverSampleRateNominal     :: Reactive Double
serverSampleRateActual      :: Reactive Double
serverUgens                 = getDatumInt     . (!! 1) <$> serverStatusDataR
serverSynths                = getDatumInt     . (!! 2) <$> serverStatusDataR
serverGroups                = getDatumInt     . (!! 3) <$> serverStatusDataR
serverInstruments           = getDatumInt     . (!! 4) <$> serverStatusDataR
serverCPUAverage            = getDatumFloat   . (!! 5) <$> serverStatusDataR
serverCPUPeak               = getDatumFloat   . (!! 6) <$> serverStatusDataR
serverSampleRateNominal     = getDatumDouble  . (!! 7) <$> serverStatusDataR
serverSampleRateActual      = getDatumDouble  . (!! 8) <$> serverStatusDataR

getDatumInt (Int x) = x
getDatumFloat (Float x) = x
getDatumDouble (Double x) = x


-- |
-- Print information about the server.
--
printServerStatus :: IO ()
printServerStatus = do
    status <- serverStatus
    msg <- return $ concatLines status
    putStrLn msg


--------------------------------------------------------------------------------

-- |
-- Standard server connection.
--
-- This is the same as 'withSC3' uses.
-- 
kStdServer              = openUDP kStdAddress kStdPort

kStdAddress             = "127.0.0.1"
kStdPort                = 57110

kStdSampleRate, kRealTimeMemorySize :: Num a => a
kStdSampleRate          = 44100
kRealTimeMemorySize     = 8192
kMasterVolume           = 0.3

kStdOutputSampleRate    = 44100
kStdOutputType          = "WAV"
kStdOutputFormat        = "int16"

-- FIXME hardcoded
(kInputBuses, kOutputBuses) = (10, 8)
-- (kInputBuses, kOutputBuses) = (2,2)

kAudioBuses             = 128
kControlBuses           = 4096
kAudioBusOffset         = kInputBuses + kOutputBuses
kControlBusOffset       = kInputBuses + kOutputBuses + kAudioBuses

kNumSpeakers, kOutputOffset :: Num a => a
kOutputOffset           = 0
kNumSpeakers            = 8

kSampleRate = 44100



pollR k = eventToReactive $ pollE k

eventToReactive :: Event a -> Reactive a
eventToReactive = stepper (error "eventToReactive: ")
