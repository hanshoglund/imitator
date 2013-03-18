
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

import Data.Monoid  
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Applicative
import Data.Time

import Music.Imitator.Reactive
import Music.Imitator.Sound hiding (pulse)
import Music.Imitator.Util

import Music.Imitator.Util

{-
clock = accumE 0 ((+ 0.1) <$ pulseE 0.1)
-- score = [(t,t) | t <- (+ 0.0) <$> [0..100]]
score = [(1.800000000000001,1),(2.0500000000000007,2),(2.549999999999999,3),(2.799999999999998,4),(3.2499999999999964,5),(3.949999999999994,6),(4.149999999999993,7),(4.399999999999992,8),(4.6499999999999915,9),(5.599999999999988,10),(5.799999999999987,11),(6.299999999999986,12),(6.499999999999985,13),(6.949999999999983,14),(7.649999999999981,15),(7.84999999999998,16),(8.04999999999998,17),(8.299999999999983,18),(12.600000000000044,19),(12.800000000000047,20),(12.900000000000048,21),(13.05000000000005,22),(13.250000000000053,23),(13.400000000000055,24),(13.550000000000058,25),(13.850000000000062,26),(14.150000000000066,27),(14.45000000000007,28),(14.650000000000073,29),(14.800000000000075,30),(14.950000000000077,31),(15.10000000000008,32),(15.250000000000082,33),(15.400000000000084,34),(15.700000000000088,35),(16.050000000000093,36)]


-- score = []

rotateMouse :: UGen -> UGen
rotateMouse gen =
    decode kNumSpeakers 
        $ foaRotate ((fst mouse + 1) * tau + (tau/8)) 
        $ foaPanB 0 0 
        $ gen

-}



-- type Time     = Double
type Duration = Time
type Envelope = Double -> Double
type Angle    = Double

data Transformation
    = Rotate Angle
    | Push
    -- TODO envelope
    -- TODO ATK rotation etc

data Command
    = StartRecord
        -- begin filling buffer from time 0
    | PauseRecord
        -- pause recording
    | ResumeRecord
        -- resume from paused position
    | StopRecord
        -- stop recording                      
    | ReadBuffer FilePath
        -- read input from given file
    | Play  Time Duration Transformation
        -- Play t d e
        -- Plays from time t to time t+d, using the given transformation


{-


runCommand :: Command -> IO ()
runCommand = undefined

runImitator :: [(Time, Command)] -> IO ()
runImitator []     = return ()
runImitator ((t,x):xs) = do
    -- usleep (round t*1000000)
    -- TODO cross-platform
    runCommand x
    runImitator xs

                                -}
