
{-# LANGUAGE OverloadedStrings #-}

module Score (
        cmdScore
  ) where

import Music.Score
import Music.Imitator
import Data.Semigroup
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.VectorSpace
import Data.AffineSpace



-- FIXME duration must be shorter than env start time

cmdScore :: Score Command
cmdScore = mempty
    |> (note $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff")
    |> sp1^*8
    |> sp2^*8

sp1 = mempty
    <> rest^*0.0 |> (note $ PlayBuffer nd 50 7 0.4 Standard (0.0 + 0  ))
    <> rest^*0.2 |> (note $ PlayBuffer nd 50 7 0.4 Standard (0.0 - 0.2))
    <> rest^*0.4 |> (note $ PlayBuffer nd 50 7 0.4 Standard (0.0 - 0.2))
    <> rest^*0.6 |> (note $ PlayBuffer nd 50 7 0.4 Standard (0.0 + 0  ))

sp2 = mempty
    <> rest^*0.0 |> (note $ PlayBuffer nd 80 7 0.4 Standard (0.5      ))
    <> rest^*0.2 |> (note $ PlayBuffer nd 80 7 0.4 Standard (0.5 + 0.2))
    <> rest^*0.4 |> (note $ PlayBuffer nd 80 7 0.4 Standard (0.5 - 0.2))
    <> rest^*0.6 |> (note $ PlayBuffer nd 80 7 0.4 Standard (0.5 + 0.3))
    <> rest^*0.8 |> (note $ PlayBuffer nd 80 7 0.4 Standard (0.5 - 0.3))


nd = 0





















main :: IO ()
main = rt

nrt = do
    writeSynthDefs
    runImitatorNRT (scoreToTrack cmdScore)

rt = do 
    startServer
    threadDelay 1000000
    runImitatorRT (scoreToTrack cmdScore)

tau = pi * 2

type Diagram = ()
cmdsToSvg :: Track Command -> Diagram
cmdsToSvg = undefined

