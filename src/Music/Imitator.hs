
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

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
-- Imitator is a program and library that performs \"spacial looping\" using the 
-- SuperCollider (scsynth) bindings for Haskell.
--
-------------------------------------------------------------------------------------

module Music.Imitator (
        module Music.Score,
    
        -- ** Commands
        -- Envelope,
        Angle,
        Turns,
        angleToTurns,
        turnsToAngle,
        Volume,
        Curve(..),
        Transformation,
        Command(..),

        -- * Running
        imitatorRT,
        imitatorNRT,

        -- ** Control
        stopServer,
        startServer,
        writeSynthDefs,
        runImitatorRT,
        runImitatorNRT,
  ) where

{-
    GUI:
    
        * Window
            * Button: Prepare
            * Button: Start
            * Button: Pause
            
            * Slider: Position
            * Text: Section, Bar

-}

import Data.Maybe
import Data.Either
import Data.Monoid  
import Control.Monad
import Control.Applicative
import System.Directory
import System.IO.Unsafe
import Math.Tau

import Control.Reactive
import Control.Reactive.Midi
import Control.Reactive.Osc
import Control.Concurrent (forkIO, threadDelay)

import Music.Score
import Music.Imitator.Sound
import Music.Imitator.Util

import qualified Data.List as List
import Sound.SC3.Server.Synthdef (synthdef, synthdefWrite) -- TODO move
import qualified Sound.SC3.Server.FD        as S           -- TODO move
import qualified Sound.SC3.UGen             as U           -- TODO move


-------------------------------------------------------------------------------------
-- Audio commands
-------------------------------------------------------------------------------------


-- Angle, in radians
newtype Angle    = Angle { getAngle :: Double }
    deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac, Floating)

-- Angle in whole circles (1 circle = tau rad)
newtype Turns    = Turns { getTurns :: Double }
    deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac, Floating)
turnsToAngle (Turns x) = Angle (x*tau)
angleToTurns (Angle x) = Turns (x/tau)

newtype Volume   = Volume { getVolume :: Double }
    deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac)

data Curve
    = Sharp
    | Short
    | Standard
    | Smooth
    | Slow
    deriving (Eq, Ord, Show, Enum)

data Transformation
    = Rotate Angle
    | Push
    -- TODO envelope
    -- TODO ATK rotation etc

data Command
    = StartRecord               -- ^ Start recording
    | StopRecord                -- ^ Stop recording
    | ReadBuffer FilePath       -- ^ Replace entire buffer with file
    | PlayBuffer 
        Int         -- node, ignore this
        Time        -- start pos in seconds
        Duration    -- end pos in seconds
        Volume      -- volume, 0 to 1
        Curve       -- envelope, 0..2
        Turns       -- angle in rotations (front is 0, left is 0.25, back is 0.5)

                               
-------------------------------------------------------------------------------------
-- Audio generators
-------------------------------------------------------------------------------------

-- |
-- Record to buffer.
--
recordG :: UGen
recordG = recordBuf bx 0 0 1 mainInput
    where
        mainInput    = (input an ax)
        (ax, an)     = kMainInputBus
        (bx, bc, bf) = kMainBuffer

-- |
-- Play a single slice to B-format bus.
--
playG :: UGen
playG = output sfx $ envelope $ spatialization $ getChannel 0 $ bufferOut
    where
        startPos        = control "time"     410
        duration        = control "duration" 1
        volumeCtrl      = control "volume"   0.5
        envelopeIndex   = control "curve"    2
        azimuth         = control "azimuth"  0

        envelopes = [
                envSust 0.0 [(0.05, 1, EnvLin)] 
                            [(0.1,  0, EnvLin)], -- sharp

                envSust 0.0 [(0.2,  1, EnvLin)]
                            [(0.4,  0, EnvLin)],

                envSust 0.0 [(1.8,  1, EnvLin)]  -- standard
                            [(2.5,  0, EnvLin)],

                envSust 0.0 [(3.0,  1, EnvLin)]  -- smooth
                            [(4.0,  0, EnvLin)],

                envSust 0.0 [(5.0,  1, EnvLin)]  -- super smooth
                            [(5.0,  0, EnvLin)]
            ]
        
        envelope :: UGen -> UGen
        envelope    = (*) $ sum $ fmap g (zip [0..] envelopes)
            where
                g (n, env) = envGen t env where t = trig * (n U.==* envelopeIndex)        
                trig       = sine (1/(2*duration))

        spatialization :: UGen -> UGen
        spatialization  = foaPanB azimuth 0                

        bufferOut :: UGen
        bufferOut    = (playBuf bc bx 0 1 (startPos*kSampleRate)) * volumeCtrl * kGlobalBufferGain

        (bx, bc, bf) = kMainBuffer
        (cx,  cn)    = kMainOutputBus
        (sfx, sfn)   = kSoundFieldBus

-- |
-- Read from output B-format bus, write to output buffers
--
decodeG :: UGen
decodeG = output cx $ decode cn (input sfn sfx)
    where          
        (sfx, sfn) = kSoundFieldBus
        (cx,  cn)  = kMainOutputBus


-------------------------------------------------------------------------------------
-- Server messages
-------------------------------------------------------------------------------------

loadSynthDefs :: [OscMessage]
loadSynthDefs = [
        S.d_loadDir kSynthDefPath
    ]

allocateBuffers :: [OscMessage]
allocateBuffers = mempty
    <> [newBuffer index frames channels]
    where
         (index, channels, frames) = kMainBuffer
    
-- |
-- Create groups 5, 6, 7 for record, play and decode respectively.        
-- 
createGroups :: [OscMessage]
createGroups = [
         S.g_new [ (5, S.AddToTail, 0),
                   (6, S.AddToTail, 0),
                   (7, S.AddToTail, 0) ]
    ]

createRecorder :: [OscMessage]
createRecorder = [
        S.s_new "imitator-record" 10 S.AddToTail 5 []
    ]
freeRecorder :: [OscMessage]
freeRecorder = [
        S.n_free [10]
    ]

createDecoder :: [OscMessage]
createDecoder = [
        S.s_new "imitator-decode" 11 S.AddToTail 7 []
    ]
freeDecoder :: [OscMessage]
freeDecoder = [
        S.n_free [11]
    ]

createPlaySynth :: Int -> Time -> Duration -> Volume -> Curve -> Turns -> [OscMessage]
createPlaySynth node time duration volume curve azimuth = [
        S.s_new "imitator-play" (20 + node) S.AddToTail 6 
            [ ("time",      fromRational $ getTime $ time),
              ("duration",  fromRational $ getDuration $ duration),
              ("volume",    getVolume $ volume),
              ("curve",     fromIntegral $ fromEnum $ curve),
              ("azimuth",   getAngle $ turnsToAngle $ azimuth) ]
    ]
freePlaySynth :: Int -> [OscMessage]
freePlaySynth n = [
        S.n_free [20 + n]
    ]


setupServer :: [OscMessage]
setupServer = mempty
    <> loadSynthDefs
    <> allocateBuffers
    <> createGroups

setupServer2 :: [OscMessage]
setupServer2 = mempty
    <> createDecoder


translateCommand :: Command -> [OscMessage]
translateCommand StartRecord                     = createRecorder
translateCommand StopRecord                      = freeRecorder
translateCommand (ReadBuffer p)                  = [readBuffer 0 p]
translateCommand (PlayBuffer n t d vol curve az) = createPlaySynth n t d vol curve az


-------------------------------------------------------------------------------------
-- Control
-------------------------------------------------------------------------------------

-- |
-- Play back commmands as messages to @scsynth@.
--
-- Time should go from 0 to 1 during the piece.
--
-- > imitatorRT time
--
imitatorRT :: Track Command -> Reactive Time -> Event OscMessage
imitatorRT cmds time = playback time (pure msgs)
    where
        msgs = getTrack $ fixDelayBug $ prependSetup $ (listToTrack . translateCommand) =<< allocateNodes cmds

        prependSetup t = listToTrack setupServer <> delay 2 (listToTrack setupServer2) <> delay 2 t
        fixDelayBug = delay 1

-- |
-- Convert commmands to a non-realtime score for @scsynth@.
--
-- Expects total duration in seconds.
--
-- > imitatorRT dur
--
imitatorNRT :: Track Command -> NRT
imitatorNRT cmds = S.NRT $ fmap toBundle msgs
    where               
        toBundle (t,m) = Bundle (fromRational $ getTime t) [m]
        
        msgs = getTrack $ fixDelayBug $ prependSetup $ (listToTrack . translateCommand) =<< allocateNodes cmds

        prependSetup t = listToTrack setupServer <> delay 2 (listToTrack setupServer2) <> delay 2 t
        fixDelayBug = delay 1

-- |
-- Simplistic node allocator. Assures each PlayBuffer command uses a separate node index.
--
allocateNodes :: Track Command -> Track Command
allocateNodes = Track . snd . List.mapAccumL g 0 . getTrack
    where
        g s (t,PlayBuffer _ pt pd pv pc paz) = (s + 1, (t,PlayBuffer s pt pd pv pc paz))
        g s (t,cmd) = (s, (t,cmd))



-------------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------------


-- |
-- Write synthdefs where @scsynth@ can find them.
--
writeSynthDefs :: IO ()
writeSynthDefs = do
    createDirectoryIfMissing True kSynthDefPath
    writeGen "record" recordG
    writeGen "decode" decodeG
    writeGen "play"   playG
    where                 
        writeGen name gen = synthdefWrite def kSynthDefPath
            where
                def  = synthdef ("imitator-" ++ name) gen

-- | 
-- Run the imitator in realtime. Output is written to the main output bus.
--
-- Assumes that synthdefs have been written and the server started.
--
runImitatorRT :: Track Command -> IO ()
runImitatorRT track = do
    runEvent $ oscOutUdp "127.0.0.1" 57110 $ imitatorRT track time
    return ()

-- |
-- Run imitator in non-realtime. Output is written to @output.wav@.
-- 
-- Assumes that synthdefs have been written.
-- 
runImitatorNRT :: Track Command -> IO ()
runImitatorNRT track  = do                                   
    cd <- getCurrentDirectory
    runServer (imitatorNRT track) Nothing (Just $ cd ++ "/output.wav")
    return ()


--------------------------------------------------------------------------------

kMainPath       = unsafePerformIO (getAppUserDataDirectory "Imitator")
kSynthDefPath   = kMainPath ++ "/synthdefs"

-- |
-- All playback buffers are multiplied by this value.
--
kGlobalBufferGain :: (Num a, Fractional a) => a
kGlobalBufferGain = 0.9

-- |
-- Channels to use on audio interface (index, numChan)
-- 
kMainInputBus   :: (Num a, Num b) => (a, b)
kMainOutputBus  :: (Num a, Num b) => (a, b)
kSoundFieldBus  :: (Num a, Num b) => (a, b)
kMainOutputBus  = (                               0,  8)
kMainInputBus   = (fromIntegral kInputBuses     + 0,  2)
kSoundFieldBus  = (fromIntegral kAudioBusOffset + 0,  4)

-- |
-- Buffers to use (index, channels, frames)
-- 
kMainBuffer :: (Num a, Num b, Num c) => (a, b, c)
kMainBuffer = (0, 2, 48000 * 60 * 35)

--------------------------------------------------------------------------------



-- sendS c   = sendStd c >> isServerRunning
-- sendSS c  = mapM sendStd c >> isServerRunning
-- dumpNodes = sendStd $ S.g_dumpTree [(0,True)]

single :: a -> [a]
single = return

-- |
-- Create a track in which all given values happen at time zero.
-- 
listToTrack :: [a] -> Track a
listToTrack = mconcat . fmap return


