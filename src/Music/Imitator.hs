
{-# LANGUAGE OverloadedStrings #-}

module Music.Imitator where

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
    | Play Time Duration    -- ^Plays from @t@ to time @t+d@, using the given transformations.
