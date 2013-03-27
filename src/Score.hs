
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Score (
        cmdScore
  ) where

import Control.Apply.Reverse
import Data.Foldable (Foldable(..))
import Data.String
import Music.Score
import Music.Imitator
import Music.Pitch.Literal
import Music.Dynamics.Literal
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




type Note = (VoiceT NotePart (TieT (TremoloT (DynamicT (ArticulationT Double)))))

noteScore :: Score Note
noteScore = mempty
    <> setVoices vl1  (melody [c,d,e])
    <> setVoices vl2  (melody [c,d,e])
    <> setVoices vla1 (melody [c,d,e])






























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

open :: Score Note -> IO ()
open = openXml . (^/4)            

play :: Score Note -> IO ()
play = playMidiIO            

rep 0 x = mempty
rep n x = x |> rep (n-1) x

data NotePart = Vl1 | Vl2 | Vla1 | Vla2 | Vc1 | Vc2 | Db1 | Db2
    deriving (Eq, Ord, Enum)
instance IsString NotePart where
    fromString _ = Vl1
instance Show NotePart where
    show Vl1  = "Violin I"
    show Vl2  = "Violin II"
    show Vla1 = "Viola I"
    show Vla2 = "Viola II"
    show Vc1  = "Violoncello I"
    show Vc2  = "Violoncello II"
    show Db1  = "Contrabass I"
    show Db2  = "Contrabass II"
    

vl1, vl2, vla1, vla2, vc1, vc2, db1, db2 :: NotePart
vl1  = undefined
vl2  = undefined
vla1 = undefined
vla2 = undefined
vc1  = undefined
vc2  = undefined
db1  = undefined
db2  = undefined


infixr 6 </>
(</>) :: (Enum v, Eq v, v ~ Voice a, Functor s, Foldable s, Semigroup (s a), HasVoice a) => s a -> s a -> s a
a </> b = a <> modifyVoices (successor offset) b
    where
        offset = succ $ maximum $ fmap fromEnum $ (getVoices a ++ [toEnum 0])



-- successor 0 = id
-- successor 1 = succ
-- successor 2 = succ . succ 
-- etc
successor :: (Enum c, Integral a) => a -> c -> c
successor n = (!! fromIntegral n) . iterate succ


{-
instance IsPitch Double where
    fromPitch (PitchL (pc, sem, oct)) = fromIntegral $ semitones sem + diatonic pc + (oct+1) * 12
        where
            semitones = maybe 0 round
            diatonic pc = case pc of
                0 -> 0
                1 -> 2
                2 -> 4
                3 -> 5
                4 -> 7
                5 -> 9
                6 -> 11
instance IsDynamics Double where
    fromDynamics (DynamicsL (Just x, _)) = x
    fromDynamics (DynamicsL (Nothing, _)) = error "IsDynamics Double: No dynamics"
-}
