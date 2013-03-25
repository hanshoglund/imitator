
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
        -- Envelope,
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

import Control.Concurrent (forkIO, threadDelay)

import qualified Data.List as List

import Sound.SC3.Server.Synthdef (synthdef, synthdefWrite) -- TODO move
import qualified Sound.SC3.Server.FD        as S           -- TODO move
import qualified Sound.SC3.UGen             as U           -- TODO move


type Angle    = Double
type Volume   = Double
type Curve    = Int


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
        Angle       -- angle in radians (0 front, tau/4 is left, tau/2 is back etc)

                               






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
playG = output sfx $ envelope $ panning $ firstChannel $ bufferOut
    where
        startPos        = control "time"     410
        duration        = control "duration" 1
        volumeCtrl      = control "volume"   0.5
        envelopeIndex   = control "curve"    0
        azimuth         = control "azimuth"  0

        envelopes = [
                envSust 0.0 [(0.01, 1, EnvLin)] 
                            [(0.2,  0, EnvLin)], -- sharp

                envSust 0.0 [(1.0,  1, EnvLin)]  -- smooth
                            [(3.0,  0, EnvLin)],

                envSust 0.0 [(5.0,  1, EnvLin)]  -- super smooth
                            [(5.0,  0, EnvLin)]
            ]
        
        -- FIXME all of these are run on trigger, first one freeing node
        envelope     = (*) $ sum $ fmap g (zip [0..] envelopes)
            where
                g (n, env) = envGen t env where t = trigger*(n U.==* envelopeIndex)
        
        trigger      = sine (1/(2*duration))

        panning      = foaPanB azimuth 0
                
        bufferOut    = (playBuf bc bx 0 1 (startPos*kSampleRate)) * volumeCtrl * kOutVol

        (bx, bc, bf) = kMainBuffer
        (cx,  cn)    = kOutBus
        (sfx, sfn)   = kSoundFieldBus
        -- TODO write to sound field


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

createPlaySynth :: Int -> Time -> Duration -> Volume -> Curve -> Angle -> [OscMessage]
createPlaySynth node time duration volume curve azimuth = [
        S.s_new "imitator-play" (20 + node) S.AddToTail 6 
            [ ("time",      fromRational $ getTime $ time),
              ("duration",  fromRational $ getDuration $ duration),
              ("volume",    volume),
              ("curve",     fromIntegral $ curve),
              ("azimuth",   azimuth) ]
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
translateCommand (PlayBuffer n t d vol curve az) = createPlaySynth n t d vol curve az


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
        g s (t,PlayBuffer _ pt pd pv pc paz) = (s + 1, (t,PlayBuffer s pt pd pv pc paz))
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


-- FIXME short durations strange (<2)
cmds :: Track Command
cmds = Track [
    -- (0,     StartRecord),
    (0,     ReadBuffer "/Users/hans/Desktop/Passager.wav"),

    (0.0,   PlayBuffer nd 350 0.2 0.1 1 (0*tau)),
    -- (0.0,   PlayBuffer nd 134 1 0.5 1 (0*tau)),
    -- (0.0,   PlayBuffer nd 238 1 0.5 1 (0*tau)),
    
    (300,   StopRecord)
    ]











--------------------------------------------------------------------------------

kOutVol = 0.7
nd = 0



--------------------------------------------------------------------------------







sendS c   = sendStd c >> isServerRunning
sendSS c  = mapM sendStd c >> isServerRunning
dumpNodes = sendStd $ S.g_dumpTree [(0,True)]

kMainPath     = unsafePerformIO (getAppUserDataDirectory "Imitator")
kSynthDefPath = kMainPath ++ "/synthdefs"

single x = [x]



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
        
        
firstChannel :: UGen -> UGen
firstChannel = head . mceChannels



--- Testing
main = do
    writeSynthDefs
    startServer
    threadDelay 1000000
    runImitatorRT

