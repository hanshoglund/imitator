
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










-- -- (index, numChan)
kInBus, kOutBus, kSoundFieldBus :: (Num a, Num b) => (a, b)
kInBus          = (0,  2)
kOutBus         = (0,  8)
kSoundFieldBus = (20, 4)

-- (index, channels, frames)
kMainBuf :: (Num a, Num b, Num c) => (a, b, c)
kMainBuf = (0, 2, 48000*60*35)

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
        (bx, bc, bf) = kMainBuf

-- |
-- Play a single slice to B-format bus.
--
playG :: UGen
playG = output 0 $ playBuf bx bc 0 1 0 
    where              
        -- TODO
        (bx, bc, bf) = kMainBuf
        (sfx, sfn)   = kSoundFieldBus

-- |
-- Read from output B-format bus, write to output buffers
--
decodeG :: UGen
decodeG = output cx $ decode cn (input sfn sfx)
    where          
        (sfx, sfn) = kSoundFieldBus
        (cx,  cn)  = kOutBus


-- |
-- Write synthdefs where @scsynth@ can find them.
--
writeSynthDefs :: IO ()
writeSynthDefs = do
    writeGen "record" recordG
    writeGen "decode" decodeG
    writeGen "play"   playG
    where                 
        writeGen name gen = synthdefWrite def path
            where
                def  = synthdef ("imitator-" ++ name) gen
                path = kSynthDefPath ++ "/imitator-" ++ name ++ ".scsyndef"

loadSynthDefs :: [OscMessage]
loadSynthDefs = mempty

createGroups :: [OscMessage]
createGroups = mempty

createStdSynths :: [OscMessage]
createStdSynths = mempty

allocateBuffers :: [OscMessage]
allocateBuffers = mempty
    <> (single $ newBuffer index channels frames)
    where
         (index, channels, frames) = kMainBuf
    


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
    -- write synthdefs
    -- splice synthdef paths into score (how)?
    -- convert score to NRT
    -- runNRT
    return ()




kMainPath     = unsafePerformIO (getAppUserDataDirectory "Imitator")
kSynthDefPath = kMainPath ++ "/synthdefs"

single x = [x]
