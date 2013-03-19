
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

import Music.Score

import Music.Imitator.Reactive
import Music.Imitator.Reactive.Midi
import Music.Imitator.Reactive.Osc
import Music.Imitator.Sound hiding (pulse)
import Music.Imitator.Util

import Music.Imitator.Util

{-
-- score = []

rotateMouse :: UGen -> UGen
rotateMouse gen =
    decode kNumSpeakers 
        $ foaRotate ((fst mouse + 1) * tau + (tau/8)) 
        $ foaPanB 0 0 
        $ gen
-}

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

-- |
-- Write synthdefs where @scsynth@ can find them.
--
writeSynthDefs :: IO ()
writeSynthDefs = undefined

-- index, num channels
kInBus   = (0,  2)
kBFBus   = (20, 4)
kOutBus  = (0,  8)
kMainBuf = (0,  2)

-- |
-- Record to buffer.
--
recordG :: UGen
recordG = input an ai
    where
        (an, ai) = kInBus
        (bn, bi) = kMainBuf

-- |
-- Read from output B-format buffer, write to output buffers
--
decodeG :: UGen
decodeG = undefined

-- |
-- Play a single slice
--
playG :: UGen
playG = undefined




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

