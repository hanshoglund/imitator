
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
        phasor,
        pulse',
        saw,
        -- vibrato,
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

        -- ** Multichannel
        (!),
        U.mce,
        U.mce2,
        U.mce3,
        U.mceChannels,
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
        -- ** Play and stop
        play,
        abort,
        sendStd,      
        
        -- ** Control

        -- ** Buffer allocation
        newBuffer,
        readBuffer,
        closeBuffer,

        -- ** Non real-time
        NRT,
        runServer,

        -- ** Real-time
        startServer,
        stopServer,
        isServerRunning,
        serverStatus,
        serverStatusData,
        serverUgens,
        serverSynths,
        serverGroups,
        serverInstruments,
        serverCPUAverage,
        serverCPUPeak,
        serverSampleRateNominal,
        serverSampleRateActual,

        printServerStatus,
        
        -- ** Constants
        kStdServer,
        kStdPort,
        kMasterVolume,
        kOutputOffset,
        kNumSpeakers,        
  ) where

import Data.Monoid
import Data.Functor.Apply
import Control.Exception ( try, SomeException )
import Control.Applicative
import Control.Concurrent ( threadDelay )

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
-- > phasor trig rate start end resetPos                 
--
phasor :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
phasor trig rate start end resetPos = U.phasor AR trig rate start end resetPos

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


foaOmni :: UGen -> UGen
foaOmni input = U.mce $ replicate 4 input

foaPanB :: UGen -> UGen -> UGen -> UGen
foaPanB azimuth elev input = U.mkFilter "FoaPanB" [input, azimuth, elev] 4

decode :: Int -> UGen -> UGen
decode numSpeakers input = U.decodeB2 numSpeakers w x y 0
    where
        [w,x,y,_] = U.mceChannels input


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


-- |
-- Multichannel duplication.
(!) :: UGen -> Int -> UGen
g ! n = U.mce $ concat $ replicate n (U.mceChannels g)

numChannels :: UGen -> Int
numChannels = length . U.mceChannels





{-
,U "FoaAsymmetry" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) asymmetry transformer"
,U "FoaDirectO" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDominateX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaDominateY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaDominateZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaFocusX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaFocusY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaFocusZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaNFC" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "distance" (1) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) nearfield compensation filter"
,U "FoaPanB" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "azimuth" (0) Nothing,I (2,2) "elevation" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) panner"
,U "FoaPressX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaPressY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaPressZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaProximity" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "distance" (1) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) proximity effect filter"
,U "FoaPsychoShelf" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "freq" (400) Nothing,I (2,2) "k0" (0) Nothing,I (3,3) "k1" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) psychoacoustic shelf filter"
,U "FoaPushX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaPushY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaPushZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaRotate" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaTilt" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaTumble" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaZoomX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
,U "FoaZoomY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
,U "FoaZoomZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
-}


newBuffer :: Int -> Int -> Int -> Message
newBuffer = undefined

readBuffer :: Int -> String -> Int -> Int -> Message
readBuffer = undefined

closeBuffer :: Int -> Message
closeBuffer = undefined












-- |
-- Add a generator to the synthesis graph.
--
play :: UGen -> IO ()
play gen = do
    asyncStd $ recvMsg
    sendStd  $ addToTailMsg
        where           
            recvMsg         = S.d_recv $ S.synthdef "playGen" (U.out kOutputOffset $ gen * kMasterVolume)
            addToTailMsg    = S.s_new "playGen" 111 S.AddToTail 0 []

-- |
-- Abort all current synthesis (remove generators from the synthesis graph).
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
runServer :: NRT -> FilePath -> FilePath -> IO ()
runServer cmds input output = do
    writeNRT scorePath cmds
    execute "scsynth" [
        "-v",   "1",
        "-N",   scorePath, 
                input, 
                output, 
                show kStdOutputSampleRate, 
                kStdOutputType, 
                kStdOutputFormat
        ]
    where
        scorePath = "score.osc"




-- |
-- Start the server.
--
startServer :: IO ()
startServer = execute "scsynth" [
    "-v", "1", 
    "-u", show kStdPort,
    "-S", show kStdSampleRate,
    "-m", show kRealTimeMemorySize
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
    status <- try serverStatus :: IO (Either SomeException [String])
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

serverUgens                 :: IO Int
serverSynths                :: IO Int
serverGroups                :: IO Int
serverInstruments           :: IO Int
serverCPUAverage            :: IO Double
serverCPUPeak               :: IO Double
serverSampleRateNominal     :: IO Double
serverSampleRateActual      :: IO Double
serverUgens                 = serverStatusData >>= return . getDatumInt . (!! 1)
serverSynths                = serverStatusData >>= return . getDatumInt . (!! 2)
serverGroups                = serverStatusData >>= return . getDatumInt . (!! 3)
serverInstruments           = serverStatusData >>= return . getDatumInt . (!! 4)
serverCPUAverage            = serverStatusData >>= return . getDatumFloat . (!! 5)
serverCPUPeak               = serverStatusData >>= return . getDatumFloat . (!! 6)
serverSampleRateNominal     = serverStatusData >>= return . getDatumDouble . (!! 7)
serverSampleRateActual      = serverStatusData >>= return . getDatumDouble . (!! 8)

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

kNumSpeakers, kOutputOffset :: Num a => a
kOutputOffset           = 0
kNumSpeakers            = 8

