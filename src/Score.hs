
{-# LANGUAGE OverloadedStrings #-}

module Score (
        mainScore
  ) where

import Music.Score
import Music.Imitator
import Control.Monad
import Control.Concurrent (threadDelay)




-- FIXME duration must be shorter than env start time

mainScore :: Track Command
mainScore = join $ Track [
    -- (0,   return $ StartRecord)
    (0,     return $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff"),
    (0,     sp1),
    (10,    sp1),
    (19,    sp1),
    (26,    sp2),
    (34,    sp2)
    -- (300,   return $ StopRecord)
    ]

sp1 = Track [
    (0.0,   PlayBuffer nd 50 7 0.4 3 (0    * tau)),
    (0.2,   PlayBuffer nd 50 7 0.4 3 (-0.2 * tau)),
    (0.4,   PlayBuffer nd 50 7 0.4 3 (-0.2 * tau)),
    (0.6,   PlayBuffer nd 50 7 0.4 3 (0    * tau))
    ]

sp2 = Track [
    (0.0,   PlayBuffer nd 80 7 0.4 3 (0    * tau)),
    (0.2,   PlayBuffer nd 80 7 0.4 3 (0.2  * tau)),
    (0.4,   PlayBuffer nd 80 7 0.4 3 (-0.2 * tau)),
    (0.6,   PlayBuffer nd 80 7 0.4 3 (0.4  * tau)),
    (0.8,   PlayBuffer nd 80 7 0.4 3 (-0.4 * tau))
    ]

nd = 0




















main :: IO ()
main = do
    writeSynthDefs
    runImitatorNRT mainScore
    
    -- startServer
    -- threadDelay 1000000
    -- runImitatorRT mainScore

tau = pi * 2

type Diagram = ()
cmdsToSvg :: Track Command -> Diagram
cmdsToSvg = undefined

