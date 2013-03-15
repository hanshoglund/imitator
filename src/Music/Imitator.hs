
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

import Music.Imitator.Reactive
import Music.Imitator.Sound
import Music.Imitator.Util


{-

type Time     = Double
type Duration = Time
type Envelope = Double -> Double
type Transformation = ()
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
