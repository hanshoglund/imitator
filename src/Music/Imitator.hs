
{-# LANGUAGE OverloadedStrings #-}

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
        Envelope,
        Angle,
        Transformation,
        Command(..),
        imitatorRT,
        imitatorNRT,
        startServer,
        stopServer,
        writeSynthDefs,
        
        runImitatorRT,
        runImitatorNRT,
        cmds,
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

import Music.Score

import Control.Reactive
import Control.Reactive.Midi
import Control.Reactive.Osc
import Music.Imitator.Sound
import Music.Imitator.Util

import Control.Concurrent (forkIO)

import qualified Data.List as List

import Sound.SC3.Server.Synthdef (synthdef, synthdefWrite) -- TODO move
import qualified Sound.SC3.Server.FD        as S           -- TODO move
import qualified Sound.SC3.UGen             as U           -- TODO move


-- type Time     = Double
-- type Duration = Time
type Envelope = Double -> Double
type Angle    = Double

data Transformation
    = Rotate Angle
    | Push
    -- TODO envelope
    -- TODO ATK rotation etc

data Command
    = StartRecord               -- ^ Start recording
    | StopRecord                -- ^ Stop recording
    | ReadBuffer FilePath           -- ^ Replace entire buffer with file
    | PlayBuffer Int Time Duration Angle  -- ^ Plays from @t@ to time @t+d@, using the given transformations.








-- ----------------------------------------------------------------------

-- -- (index, numChan)
kInBus, kOutBus, kSoundFieldBus :: (Num a, Num b) => (a, b)
kOutBus         = (                               0,  8)
kInBus          = (fromIntegral kInputBuses     + 0,  2)
kSoundFieldBus  = (fromIntegral kAudioBusOffset + 0,  4)

-- (index, channels, frames)
kMainBuffer :: (Num a, Num b, Num c) => (a, b, c)
kMainBuffer = (0, 2, 48000 * 60 * 35)

-- ----------------------------------------------------------------------

-- |
-- Record to buffer.
--
recordG :: UGen
recordG = recordBuf bx offset trig onOff (input an ax)
    where                               
        offset   = 0
        trig     = 0
        onOff    = 1
        (ax, an)     = kInBus
        (bx, bc, bf) = kMainBuffer

-- |
-- Play a single slice to B-format bus.
--
playG :: UGen
playG = 
    -- output cx $ bufferOut
    output sfx $ foaPanB azimuth 0 $ firstChannel $ bufferOut
    where                       
        azimuth     = control "azimuth" 0
        volume      = control "volume"  0
        envelope    = control "envelope" 0
        
        bufferOut = playBuf bc bx 0 1 0 * kOutVol
        (bx, bc, bf) = kMainBuffer
        (cx,  cn)    = kOutBus
        (sfx, sfn)   = kSoundFieldBus
        -- TODO write to sound field

firstChannel = head . mceChannels

-- |
-- Read from output B-format bus, write to output buffers
--
decodeG :: UGen
decodeG = output cx $ decode cn (input sfn sfx)
    where          
        (sfx, sfn) = kSoundFieldBus
        (cx,  cn)  = kOutBus

-- ----------------------------------------------------------------------

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

-- ----------------------------------------------------------------------


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

createPlaySynth :: Int -> Angle -> [OscMessage]
createPlaySynth n azimuth = [
        S.s_new "imitator-play" (20 + n) S.AddToTail 6 [("azimuth", azimuth)]
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
translateCommand StartRecord             = createRecorder
translateCommand StopRecord              = freeRecorder
translateCommand (ReadBuffer p)          = [readBuffer 0 p]
translateCommand (PlayBuffer n t d az)   = createPlaySynth n az
-- TODO buffer allocation, params to play etc

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

-- Create a track in which all given values happen at time zero.
listToTrack :: [a] -> Track a
listToTrack = mconcat . fmap return

allocateNodes :: Track Command -> Track Command
allocateNodes = Track . snd . List.mapAccumL g 0 . getTrack
    where
        g s (t,PlayBuffer _ pt pd paz) = (s + 1, (t,PlayBuffer s pt pd paz))
        g s (t,cmd) = (s, (t,cmd))

-- |
-- Convert commmands to a non-realtime score for @scsynth@.
--
-- Expects total duration in seconds.
--
-- > imitatorRT dur
--
imitatorNRT :: Track Command -> Duration -> NRT
imitatorNRT = undefined



runImitatorRT :: IO ()
runImitatorRT = do
    runEvent $ oscOutUdp "127.0.0.1" 57110 $ imitatorRT cmds time
    return ()

-- |
-- Run over the given input file.
runImitatorNRT :: FilePath -> FilePath -> IO ()
runImitatorNRT input output = do
    writeSynthDefs
    -- convert score to NRT
    -- runNRT
    return ()



cmds :: Track Command
cmds = Track [
    -- (0,     StartRecord),
    (0,     ReadBuffer "/Users/hans/Desktop/Test/test.wav"),
    (0.5,   PlayBuffer 0  0  0  (0*tau)),
    (1.0,   PlayBuffer 0  0  0  (0.1*tau)),
    (1.5,   PlayBuffer 0  0  0  (0.2*tau)),
    (2.0,   PlayBuffer 0  0  0  (0.3*tau)),
    (2.5,   PlayBuffer 0  0  0  (0.4*tau)),
    (3.0,   PlayBuffer 0  0  0  (0.5*tau)),
    (3.5,   PlayBuffer 0  0  0  (0.6*tau)),
    (4.0,   PlayBuffer 0  0  0  (0.7*tau)),
    (4.5,   PlayBuffer 0  0  0  (0.8*tau)),
    (5.0,   PlayBuffer 0  0  0  (0.9*tau)),
    (5.5,   PlayBuffer 0  0  0 (0.10*tau)),
    (6.0,   PlayBuffer 0  0  0 (0.11*tau)),
    
    (300,   StopRecord)
    ]










--------------------------------------------------------------------------------

kOutVol = 0.7



--------------------------------------------------------------------------------







sendS c   = sendStd c >> isServerRunning
sendSS c  = mapM sendStd c >> isServerRunning
dumpNodes = sendStd $ S.g_dumpTree [(0,True)]

kMainPath     = unsafePerformIO (getAppUserDataDirectory "Imitator")
kSynthDefPath = kMainPath ++ "/synthdefs"

single x = [x]



