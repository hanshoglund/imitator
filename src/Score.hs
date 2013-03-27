
{-# LANGUAGE OverloadedStrings #-}

module Score (
        cmdScore
  ) where

import Control.Apply.Reverse
import Music.Score
import Music.Imitator
import Data.Semigroup
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.VectorSpace
import Data.AffineSpace



instance Monoid Command where
    mempty = PlayBuffer 0 0 1 0.5 Standard 0
    x `mappend` y = x

playFromDuring t d = setTime t $ setDur d $ note mempty

setTime  :: Time -> Score Command -> Score Command
setTime  = \x -> fmap (setTime' x)
setDur   :: Duration -> Score Command -> Score Command
setDur   = \x -> fmap (setDur' x)
setVol   :: Volume -> Score Command -> Score Command
setVol   = \x -> fmap (setVol' x)
setCurve :: Curve -> Score Command -> Score Command
setCurve = \x -> fmap (setCurve' x)
setAzim  :: Turns -> Score Command -> Score Command
setAzim  = \x -> fmap (setAzim' x)
setTime'  t (PlayBuffer n _ d v c az)  = PlayBuffer n t d v c az
setTime'  _ x                          = x
setDur'   d (PlayBuffer n t _ v c az)  = PlayBuffer n t d v c az
setDur'   _ x                          = x
setVol'   v (PlayBuffer n t d _ c az)  = PlayBuffer n t d v c az
setVol'   _ x                          = x
setCurve' c (PlayBuffer n t d v _ az)  = PlayBuffer n t d v c az
setCurve' _ x                          = x
setAzim'  az (PlayBuffer n t d v c _)  = PlayBuffer n t d v c az
setAzim'  _ x                          = x




-- FIXME duration must be shorter than env start time

cmdScore :: Score Command
cmdScore = mempty
    |> (note $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff")
    |> sp1^*8
    |> sp2^*8

sp1 = mempty
    <> rest^*0.0 |> (playFromDuring 50 7 & setAzim (0.0 + 0))
    <> rest^*0.2 |> (playFromDuring 50 7 & setAzim (0.0 - 0.2))
    <> rest^*0.4 |> (playFromDuring 50 7 & setAzim (0.0 - 0.2))
    <> rest^*0.6 |> (playFromDuring 50 7 & setAzim (0.0 + 0  ))

sp2 = mempty
    <> rest^*0.0 |> (playFromDuring 80 7 & setAzim (0.5      ))
    <> rest^*0.2 |> (playFromDuring 80 7 & setAzim (0.5 + 0.2))
    <> rest^*0.4 |> (playFromDuring 80 7 & setAzim (0.5 - 0.2))
    <> rest^*0.6 |> (playFromDuring 80 7 & setAzim (0.5 + 0.3))
    <> rest^*0.8 |> (playFromDuring 80 7 & setAzim (0.5 - 0.3))


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

