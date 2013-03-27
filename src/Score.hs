
{-# LANGUAGE OverloadedStrings #-}

module Score (
        mainScore
  ) where

import Music.Score
import Music.Imitator
import Data.Semigroup
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.VectorSpace
import Data.AffineSpace



-- FIXME duration must be shorter than env start time

mainScore :: Score Command
mainScore = mempty
    |> (note $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff")
    |> sp2^*10
    |> sp2^*10
    |> sp2^*8
    |> sp1^*8
    |> sp2^*8

sp1 = mempty
    |> rest^*0.0 |> (note $ PlayBuffer nd 50 7 0.4 3 (0    * tau))
    |> rest^*0.2 |> (note $ PlayBuffer nd 50 7 0.4 3 (-0.2 * tau))
    |> rest^*0.4 |> (note $ PlayBuffer nd 50 7 0.4 3 (-0.2 * tau))
    |> rest^*0.6 |> (note $ PlayBuffer nd 50 7 0.4 3 (0    * tau))

sp2 = mempty
    |> rest^*0.0 |> (note $ PlayBuffer nd 80 7 0.4 3 ((1      ) * tau))
    |> rest^*0.2 |> (note $ PlayBuffer nd 80 7 0.4 3 ((1 + 0.2) * tau))
    |> rest^*0.4 |> (note $ PlayBuffer nd 80 7 0.4 3 ((1 - 0.2) * tau))
    |> rest^*0.6 |> (note $ PlayBuffer nd 80 7 0.4 3 ((1 + 0.3) * tau))
    |> rest^*0.8 |> (note $ PlayBuffer nd 80 7 0.4 3 ((1 - 0.3) * tau))


nd = 0





















main :: IO ()
main = rt

nrt = do
    writeSynthDefs
    runImitatorNRT (scoreToTrack mainScore)

rt = do 
    startServer
    threadDelay 1000000
    runImitatorRT (scoreToTrack mainScore)

tau = pi * 2

type Diagram = ()
cmdsToSvg :: Track Command -> Diagram
cmdsToSvg = undefined

