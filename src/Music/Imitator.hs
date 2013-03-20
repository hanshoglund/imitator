
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

import Music.Score

import Music.Imitator.Reactive
import Music.Imitator.Reactive.Midi
import Music.Imitator.Reactive.Osc
import Music.Imitator.Sound
import Music.Imitator.Util

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
    = StartRecord           -- ^ Start recording
    | StopRecord            -- ^ Stop recording
    | ReadBuffer  FilePath  -- ^ Replace entire buffer with file
    | WriteBuffer FilePath  -- ^ Write entire buffer to file
    | Play Time Duration    -- ^ Plays from @t@ to time @t+d@, using the given transformations.








-- ----------------------------------------------------------------------

-- -- (index, numChan)
kInBus, kOutBus, kSoundFieldBus :: (Num a, Num b) => (a, b)
kOutBus         = (0,                                 8)
kInBus          = (8,                                 2)
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
playG = output cx $ playBuf bc bx 0 1 0 
    where              
        (bx, bc, bf) = kMainBuffer
        (cx,  cn)    = kOutBus
        -- TODO write to sound field
        -- (sfx, sfn)   = kSoundFieldBus

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
-- Create groups 1, 2, and 3 for record, play and decode respectively.        
-- 
createGroups :: [OscMessage]
createGroups = [
         S.g_new [ (1,S.AddToTail,0),
                   (2,S.AddToTail,0),
                   (3,S.AddToTail,0) ]
    ]

createRecorder :: [OscMessage]
createRecorder = [
        S.s_new "imitator-record" 10 S.AddToTail 1 []
    ]
freeRecorder :: [OscMessage]
freeRecorder = [
        S.n_free [10]
    ]

createDecoder :: [OscMessage]
createDecoder = [
        S.s_new "imitator-decode" 11 S.AddToTail 3 []
    ]
freeDecoder :: [OscMessage]
freeDecoder = [
        S.n_free [11]
    ]

createPlaySynth :: Int -> [OscMessage]
createPlaySynth n = [
        S.s_new "imitator-play" (20 + n) S.AddToTail 2 []
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
    <> createDecoder


translateCommand :: Command -> [OscMessage]
translateCommand cmd = mempty



-- |
-- Play back commmands as messages to @scsynth@.
--
-- Time should go from 0 to 1 during the piece.
--
-- > imitatorRT time
--
imitatorRT :: Track Command -> Reactive Time -> Event OscMessage
imitatorRT = mempty

-- |
-- Convert commmands to a non-realtime score for @scsynth@.
--
-- Expects total duration in seconds.
--
-- > imitatorRT dur
--
imitatorNRT :: Track Command -> Duration -> NRT
imitatorNRT = undefined




-- |
-- Run over the given input file.
runImitatorNRT :: FilePath -> FilePath -> IO ()
runImitatorNRT input output = do
    writeSynthDefs
    -- convert score to NRT
    -- runNRT
    return ()


sendS c   = sendStd c >> isServerRunning
sendSS c  = mapM sendStd c >> isServerRunning
dumpNodes = sendStd $ S.g_dumpTree [(0,True)]




kMainPath     = unsafePerformIO (getAppUserDataDirectory "Imitator")
kSynthDefPath = kMainPath ++ "/synthdefs"

single x = [x]
