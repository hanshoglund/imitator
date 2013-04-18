
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, OverloadedStrings #-}

module Score (
        cmdScore,
        notePos,
        noteScore
  ) where

import Control.Monad
import Control.Apply.Reverse
import Control.Concurrent (threadDelay)
import Math.Tau
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Foldable (Foldable(..), toList)
import Data.Ord (comparing)
import Data.String
import Data.Ratio
import Data.NumInstances
import qualified Data.List as List

import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score
import Music.Score.Combinators
import Music.Score.Rhythm (quantize)

import Music.Imitator
import Diagrams.Prelude hiding (open, duration, stretch, stretchTo, (|>), Time, Duration, (&), text, e, tau)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString


--------------------------------------------------------------------------------

-- FIXME duration must be shorter than env start time
-- TODO spread out source positions (minimize risk of noise etc)

cmdScore :: Score Command
cmdScore = cmdScore' False

cmdScore' :: Bool -> Score Command
cmdScore' rt = mempty
    -- <> if rt
    --     then (delay 0 $ note StartRecord)
    --     else (mempty
    --         <> delay 0 (readBuffer "/Users/hans/Documents/Kod/hs/music-imitator/sounds/test.aiff")
    --         <> delay 0 (playOnce 0 1800 & setCurve Sharp & setAzim (0.0 + 0))            
    --     )
    <> note StartRecord

    <> delay (12*4)     echoShort1
    <> delay (57*4)     echoShort2

    <> delay (37*4)     echoCanon_I
    <> delay (42*4)     echoCanon_I

    <> delay (88*4)     echoMiddleC1
    <> delay (92*4)     echoMiddleC1
    <> delay (96*4)     echoMiddleC1
    <> delay (100*4)    echoMiddleC2
    <> delay (104*4)    echoMiddleC2
    <> delay (108*4)    echoMiddleC2

    <> delay (115*4)    echoCanon_IIa
    <> delay (138*4)    echoCanon_IIb

    <> delay (166*4)    echoMiddleF1
    <> delay (170*4)    echoShort1Mini
    <> delay (180*4)    echoMiddleF2

    <> delay (198*4)    echoCanon_III
--    <> delay (230*4)    echoCanon_III
    <> delay (260*4)    echoShort1Mini

    -- break here?
    
    -- canon_IV
    <> delay (293*4)    echoCanon_IV

    <> delay (400*4+15)    echoLongNote
    <> delay (400*4+20+30) echoEnd

    <> delay (duration noteScore) (note StopRecord) -- mark end

echoCanon_I = mempty
    |> (playOnce (28*4) (25*4) & setCurve Smooth & setAzim (0.0 + 0.3))
    |> rest^*(4*8)
    |> (playOnce (28*4) (25*4) & setCurve Smooth & setAzim (0.0 - 0.3))

echoCanon_IIa = mempty
    |> (playOnce (107*4) (20*4) & setCurve Smooth & setAzim (0.0 + 0.15))
echoCanon_IIb = mempty
    |> (playOnce (107*4) (20*4) & setCurve Smooth & setAzim (0.0 - 0.15))

echoCanon_III = mempty
    |> (playOnce (191*4) (30*4) & setCurve Smooth & setAzim (0.0 + 0.2))
    |> rest^*(8*8)
    |> (playOnce (191*4) (30*4) & setCurve Smooth & setAzim (0.0 - 0.2))
    |> rest^*(8*8)
    |> (playOnce (191*4) (30*4) & setCurve Smooth & setAzim (0.0 + 0.5))
    |> rest^*(8*8)

echoCanon_IV = mempty
    |> (playOnce (296*4) (60*4) & setCurve Smooth & setAzim (0.0 + 0.2))
    |> rest^*(4*16)
    |> (playOnce (304*4) (60*4) & setCurve Smooth & setAzim (0.0 - 0.2))
    |> rest^*(4*16)
    |> (playOnce (312*4) (60*4) & setCurve Smooth & setAzim (0.0 + 0.4))
    |> rest^*(4*16)
    |> (playOnce (312*4) (50*4) & setCurve Smooth & setAzim (0.0 - 0.4))
    |> rest^*(4*16)
    |> (playOnce (312*4) (40*4) & setCurve Smooth & setAzim (0.0 - 0.5))

echoShort1Mini = mempty
    |> (playOnce 10  50 & setCurve Smooth & setAzim (0 + 0.1))
    |> rest^*10
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0 - 0.1))
    |> rest^*10

echoShort1 = mempty
    |> (playOnce 10  50 & setCurve Smooth & setAzim (0 + 0.1))
    |> rest^*10
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0 - 0.1))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0 + 0.2))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0 - 0.2))
    |> rest^*10
    |> (playOnce 30 50 & setCurve Smooth & setAzim (0 + 0.3))
    |> rest^*10
    |> (playOnce 30 50 & setCurve Smooth & setAzim (0 - 0.3))
    |> rest^*10

echoShort2 = mempty
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0 + 0.5))
    |> rest^*10
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0 - 0.35))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0 + 0.35))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0 - 0.25))
    |> rest^*10
    |> (playOnce 30 50 & setCurve Smooth & setAzim (0 + 0.25))
    |> rest^*10
    |> (playOnce 30 50 & setCurve Smooth & setAzim (0 - 0.0))
    |> rest^*10


echoMiddleC1 = mempty
    |> (playOnce (87*4)  (8*4) & setCurve Smooth & setAzim (0 + 0.4))
echoMiddleC2 = mempty
    |> (playOnce (100*4) (8*4) & setCurve Smooth & setAzim (0 - 0.4))


echoMiddleF1 = mempty
    |> (playOnce (161*4) (12*4) & setCurve Smooth & setAzim (0 + 0.2))
echoMiddleF2 = mempty
    |> (playOnce (176*4) (12*4) & setCurve Smooth & setAzim (0 - 0.2))


-- dur ca 200
echoLongNote = repTimes 30 (capture |> rest^*5)
    where
        capture = playOnce (400*4+5) 15 & setCurve Smooth & setAzim (0 + 0.0)

echoEnd = mempty
    |> (playOnce (400*4+20+10) (60*3) & setCurve Smooth & setAzim (0 + 0.5))
    |> rest^*30
    |> (playOnce (400*4+20+20) (60*3) & setCurve Smooth & setAzim (0 - 0.35))
    |> rest^*30
    |> (playOnce (400*4+20+30) (60*3) & setCurve Smooth & setAzim (0 + 0.35))
    |> rest^*30
    |> (playOnce (400*4+20+40) (60*3) & setCurve Smooth & setAzim (0 - 0.25))
    |> rest^*30
    |> (playOnce (400*4+20+10) (60*3) & setCurve Smooth & setAzim (0 + 0.25))
    |> rest^*30
    |> (playOnce (400*4+20+10) (60*3) & setCurve Smooth & setAzim (0 + 0))
    |> rest^*30


--------------------------------------------------------------------------------

-- Get (bar,beat)
-- 400 bars in 60 BPM
-- 20 seconds in bar 401
-- 45 bars (to 446)
-- 30 seconds in bar 447       

notePos :: Int -> (Int,Int)
notePos t |                 t < 1600  =  ( t `div` 4 + 1 , t `mod` 4   + 1)
          | 1600    <= t && t < 1620  =  ( 401           , t - (400*4)  + 1)
          | 400*4+20 <= t && t < 445*4+20  =  ( (t-20) `div` 4 + 2, (t-20) `mod` 4 + 1 )
          | otherwise                     =  447

noteScore :: Score Note
noteScore = {-addInstrChange $-}

    -- * Part 1 (first canon and col legno)
        (colLegno1  </> delay (4*3) colLegno1) 
    ||> (canon_I <> (delay (4*5) $ moveToPart vl2 $ canon_I))     -- A
    ||> (colLegno2  </> delay (4*3) colLegno2)                  -- B
    
    -- * Part 2 (canon_II and surrounding)
    -- C
    ||> (dynamics _p $ bar^*30
            <> delay 0      (moveToPart vc2 g_^*(4*13))
            <> delay (4*15) (moveToPart vc1 a_^*(4*13))
            )
    -- -- D, E
    ||> canon_II
    ||> (bar^*15 <> moveToPart vl2 (rev canon_II))
    -- -- F
    ||> (dynamics _p $ bar^*30
            <> delay 0      (moveToPart vc2 bb_^*(4*15))
            <> delay (4*15) (moveToPart vc1 c  ^*(4*15))
            )
    ||> (canon_III <> (delay (4*30) $ moveToPart vl2 $ canon_III))     -- A
    
    
    -- * Part 3 (development to canon_IV)
    -- I
    ||> (mempty
            <> delay 0      (moveToPart vl1  f'  ^*(4*15))     
            <> delay (4*15) (moveToPart vl2  f'  ^*(4*15)) 
            )
    -- K
    ||> bar^*1
    ||> canon_IV
    
    ||> rest^*20
    
    -- * Part 4 (jete)
    -- FIXME sync back to score
    ||> mconcat [
            delay 0 $ dynamics ppp $ up (12*3) $ moveToPart vl2  $ d_^*(4*30),
            delay (4*10) (dynamics _p $ jete1 </> delay (12*8) jete1)
           ]
    ||> bar^*2
    ||> c'^*4 -- mark ending!


--------------------------------------------------------------------------------

colLegno1 :: Score Note
colLegno1 = {-staccato $ -} dynamics (ppp `cresc` mp |> mp^*0.2) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repTimes 7 $ [4,4,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ repTimes 7 $ [4,4,5,4,5] `groupWith` g |> rest^*6)
    </> (down 24 $ delay 3 $ repTimes 7 $ [4,5,4,5,4] `groupWith` g |> rest^*6)
    </> (down 24 $ delay 6 $ repTimes 7 $ [3,3,5,3,5] `groupWith` g |> rest^*6)

-- dur 45

colLegno2 :: Score Note
colLegno2 = {-staccato $ -} dynamics (mp) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repTimes 4 $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ repTimes 4 $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 3 $ repTimes 4 $ [4,5,4,5,4,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 6 $ repTimes 4 $ [3,3,5,3,3]    `groupWith` g |> rest^*6)

colLegno2Alt :: Score Note
colLegno2Alt = {-staccato $ -} dynamics (mp) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repWithIndex 4 $ \t -> [4,4,5,4,5,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down 12 $ delay 1 $ repWithIndex 4 $ \t -> [4,4,5,4,5,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down 24 $ delay 3 $ repWithIndex 4 $ \t -> [4,5,4,5,4,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down 24 $ delay 6 $ repWithIndex 4 $ \t -> [3,3,5,3,3]    `groupWith` g |> rest^*(1+4*t))

--------------------------------------------------------------------------------

makeJete :: Pitch Note -> Bool -> Duration -> Score Note
makeJete p v d = text "jeté" $ modifyPitches (+ p) $ g_ |> ((if v then cs else cs_){-^/2-}) {-|> rest^/2-} |> rest^*d

makeJetes :: [Pitch Note] -> [Bool] -> [Duration] -> Score Note
makeJetes ps vs ds = scat $ zipWith3 makeJete ps vs ds

jete1 :: Score Note
jete1 = (rest <>) $ -- FIXME temporary fix w.r.t onset/padToBar
        (delay 3  $ up 0    $ makeJetes (rotated 0 ps) (rotated 3 vs) (rotated 1 ds))
    </> (delay 5  $ up 0    $ makeJetes (rotated 1 ps) (rotated 0 vs) (rotated 3 ds))^*(4/5)
    </> (delay 7  $ down 12 $ makeJetes (rotated 2 ps) (rotated 1 vs) (rotated 2 ds))
    </> (delay 12 $ down 12 $ makeJetes (rotated 3 ps) (rotated 2 vs) (rotated 0 ds))^*(4/5)
    where
        ps = take n $ cycle [0,6,6,0,6,6,0]
        vs = take n $ cycle [True,False,True,False,True,False,True,False]
        ds = take n $ cycle $ fmap (+ 4) [3,7,5,7,5,5,3,7,7,7,7,7,5,3,7,7,7,7,7,3,3,5]
        n  = 9

-- colLegno3 :: Score Note
-- colLegno3 = (down 12 $ delay 0 $ repeatS $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)


--------------------------------------------------------------------------------

makeCanon_I :: Double -> Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon_I n dn subj1 subj2 =
        dynamics dn (rev (a </> b </> c </> d) |> (a </> b </> c </> d))
    where
        a = (repTimes (5*n/(4/3)) $ subj1 ^*(4/3))
        b = (repTimes (5*n/1)     $ subj2 ^*1)
        c = (repTimes (5*n/2)     $ subj1 ^*2)
        d = (repTimes (5*n/3)     $ subj2 ^*3)

canon_I :: Score Note
canon_I = text "ord" $ (^*2) $ makeCanon_I 1 dn subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> g_^*2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*3
        dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

makeCanon_II :: Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon_II dn subj1 subj2 =
        dynamics dn (rev $ a </> b </> c </> d)
    where
        a = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*(4/3))
        b = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*1)
        c = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*2)
        d = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*3)

canon_II :: Score Note
canon_II = text "ord" $ (^*2) $ makeCanon_II dn subj1 subj2
    where
        subj1 = g_ |> d^*(3/2) |> c^/2 |> a_^/2 |> bb_^/2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> d_^*3
        dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

makeCanon_III :: Double -> Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon_III n dn subj1 subj2 =
        dynamics dn (rev (a </> b </> c </> d) |> (a </> b </> c </> d))
    where
        a = (repTimes (5*n/(4/3)) $ subj1 ^*(4/3))
        b = (repTimes (5*n/1)     $ subj2 ^*1)
        c = (repTimes (5*n/2)     $ subj1 ^*2)
        d = (repTimes (5*n/3)     $ subj2 ^*3)

canon_III :: Score Note
canon_III = text "ord" $ makeCanon_III 1.6 dn subj1 subj2
    where
        subj1 = g^*2 |> d |> eb^*(3/2) |> c^*2 |> d^*2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*2 |> d^*3 |> c^*1
        dn   = (repTimes 5 $ (mf `cresc` _f)^*3 |> (_f `dim` mf)^*3 )

makeCanon_IV :: Bool -> Score Note -> Score Note -> Score Note -> Score Note
makeCanon_IV flip subj1 subj2 bass = if flip then lower </> upper else upper </> lower
    where
        upper = (repWithTime (10/(4/5)) $ \t -> reg Vl1 t   $ subj1 ^* (4/5) )
            </> (repWithTime (12/(2/3)) $ \t -> reg Vla1 t  $ subj1 ^* (2/3) )
            </> (repWithTime (15/ 1   ) $ \t -> reg Vc1 t   $ subj1 ^* 1     )
            </> (repWithTime (18/ 2   ) $ \t -> reg Db2 t   $ bass ^* 1    )

        lower = (repWithTime (10/(2/3)) $ \t -> reg Vl2 t   $ subj2 ^* (2/3) )
            </> (repWithTime (12/ 1   ) $ \t -> reg Vla2 t  $ subj2 ^* 1     )
            </> (repWithTime (15/(3/2)) $ \t -> reg Vc2 t   $ subj2 ^* (3/2) )
            </> (repWithTime (18/ 2   ) $ \t -> reg Db2 t   $ bass ^* 1    )

        reg Vl1  t | t < 0.3 = up   (octave + fifth) | t < 0.6 = up octave       | t >= 0.6 = up fifth
        reg Vl2  t | t < 0.4 = up   octave           | t < 0.7 = up fifth        | t >= 0.7 = up fifth
        reg Vla1 t | t < 0.4 = up   fifth            | t < 0.7 = up fifth        | t >= 0.7 = up unison
        reg Vla2 t | t < 0.4 = up   unison           | t < 0.7 = up fifth        | t >= 0.7 = up unison
        reg Vc1  t | t < 0.4 = down octave           | t < 0.7 = down octave     | t >= 0.7 = down fourth
        reg Vc2  t | t < 0.4 = down octave           | t < 0.7 = down octave     | t >= 0.7 = down fourth

        reg Db1  t | t < 0.4 = down (octave*1)       | t < 0.7 = down (octave*1) | t >= 0.7 = down (octave*1)
        reg Db2  t | t < 0.4 = down (octave*2)       | t < 0.7 = down (octave*1) | t >= 0.7 = down (octave*1)


canon_IV :: Score Note
canon_IV = text "ord" $ c^*padC |> firstC |> secondC
    where
        firstC  = dynamics dn1 $ rev $ makeCanon_IV False subj1 subj2 bass
        secondC = dynamics dn2       $ makeCanon_IV True subj1 subj2 bass
        padC    = fromIntegral $ 4 - numerator (getDuration $ duration firstC) `mod` 4
        dn1     = (repTimes 10 $ (mf `cresc` _f)^*5 |> (_f `dim` mf)^*5)
        dn2     = (repTimes 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)

        subj1 = down 2 $ (d^*3 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
        subj2 = down 2 $ (d^*2 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
        bass  = melody [d,a] |> g^*2 |> melody [c,d,a] |> g^*2
        -- bass  = melody [d,g] |> a^*2 |> melody [c,g,d] |> a^*2





--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

instance Monoid Command where
    mempty = PlayBuffer 0 0 1 0.5 Standard 0
    x `mappend` y = x

readBuffer = note . ReadBuffer
playOnce t d = setTime t $ setDur d $ note mempty

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

--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

data NotePart
    = Vl1
    | Vla1
    | Vc1
    | Db1
    | Vl2
    | Vla2
    | Vc2
    | Db2
    deriving (Eq, Ord, Enum)

instance IsString NotePart where
    fromString _ = Vl1

instance Show NotePart where
    show Vl1  = "Violin 1"
    show Vl2  = "Violin 2"
    show Vla1 = "Viola 1"
    show Vla2 = "Viola 2"
    show Vc1  = "Violoncello 1"
    show Vc2  = "Violoncello 2"
    show Db1  = "Double Bass 1"
    show Db2  = "Double Bass 2"
-- instance Show NotePart where
--     show Vl1  = "Vl. 1"
--     show Vl2  = "Vl. 2"
--     show Vla1 = "Vla. 1"
--     show Vla2 = "Vla. 2"
--     show Vc1  = "Vc. 1"
--     show Vc2  = "Vc. 2"
--     show Db1  = "Db. 1"
--     show Db2  = "Db. 2"

vl1, vl2, vla1, vla2, vc1, vc2, db1, db2 :: NotePart
vl1  = Vl1
vl2  = Vl2
vla1 = Vla1
vla2 = Vla2
vc1  = Vc1
vc2  = Vc2
db1  = Db1
db2  = Db2

-- 1 or 2
getPartGroup :: NotePart -> Int
getPartGroup p = case p of
    Vl1    -> 1
    Vla1   -> 1
    Vc1    -> 1
    Db1    -> 1
    _      -> 2

-- A hack, works only in Sibelius
addInstrChange :: Score Note -> Score Note
addInstrChange = mapVoices $
    \[a,b,c,d,e,f,g,h] ->
        [ text "~P41" a,
          text "~P42" b,
          text "~P43" c,
          text "~P44" d,
          text "~P41" e,
          text "~P42" f,
          text "~P43" g,
          text "~P44" h
          ]

type Note = (VoiceT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))

score :: Score Note -> Score Note
score = id

open :: Score Note -> IO ()
open = openXml . (^/4)

play :: Score Note -> IO ()
play = playMidiIO

simple :: Score (VoiceT Integer Integer) -> Score (VoiceT Integer Integer)
simple = id

-- quantizeScore = fmap (quantize . getPart) . scoreToParts . (^/4)

--------------------------------------------------------------------------------

main :: IO ()
main = dr

dr = do
    let dia = drawScores noteScore cmdScore
    let svg = renderDia SVG (SVGOptions (Dims 1800 800)) dia
    let bs  = renderSvg svg
    ByteString.writeFile "score.svg" bs
-- 
-- drp = do
--     let dia = drawPoster
--     let svg = renderDia SVG (SVGOptions (Dims 1800 800)) dia
--     let bs  = renderSvg svg
--     ByteString.writeFile "poster.svg" bs  

nrt = do
    writeSynthDefs
    runImitatorNRT (scoreToTrack cmdScore)

rt = do
    startServer
    threadDelay 1000000
    runImitatorRT (scoreToTrack cmdScore)











--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO move all this stuff

resetDynamics :: HasDynamic c => c -> c
resetDynamics = setBeginCresc False . setEndCresc False . setBeginDim False . setEndDim False
-- FIXME setLevel

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0



--------------------------------------------------------------------------------
-- Pitch
--------------------------------------------------------------------------------

-- TODO better transposition etc
-- TODO interval literals (Music.Pitch.Interval.Literal)
up x = fmap (modifyPitch (+ x))
down x = fmap (modifyPitch (subtract x))

-- TODO move to Music.Pitch.Interval.Literal
unison     = 0
octave     = 12
tritone    = 6
fifth      = 7
fourth     = 5
minorThird = 3
majorThird = 4


--------------------------------------------------------------------------------
-- Structure
--------------------------------------------------------------------------------

-- |
-- Repeat exact amount of times.
--
-- > Duration -> Score Note -> Score Note
--
repTimes :: (Enum a, Monoid c, HasOnset c, Delayable c) => a -> c -> c
repTimes n a = replicate (0 `max` fromEnum n) () `repWith` (const a)

-- |
-- Repeat once for each element in the list.
--
-- > [a] -> (a -> Score Note) -> Score Note
--
-- Example:
-- > repWith [1,2,1] (c^*)
--
repWith :: (Monoid c, HasOnset c, Delayable c) => [a] -> (a -> c) -> c
repWith = flip (\f -> scat . fmap f)

-- |
-- Combination of 'scat' and 'fmap'. Note that
--
-- > scatMap = flip repWith
--
scatMap f = scat . fmap f

-- |
-- Repeat exact amount of times with an index.
--
-- > Duration -> (Duration -> Score Note) -> Score Note
--
repWithIndex :: (Enum a, Num a, Monoid c, HasOnset c, Delayable c) => a -> (a -> c) -> c
repWithIndex n = repWith [0..n-1]

-- |
-- Repeat exact amount of times with relative time.
--
-- > Real a => a -> (Time -> Score Note) -> Score Note
--
repWithTime :: (Enum a, Fractional a, Monoid c, HasOnset c, Delayable c) => a -> (a -> c) -> c
repWithTime n = repWith $ fmap (/ n') [0..(n' - 1)]
    where
        n' = n

-- |
-- Repeat a number of times and scale down by the same amount.
--
-- > Duration -> Score Note -> Score Note
--
group :: (Enum a, Fractional a, a ~ Scalar c, Monoid c, Semigroup c, VectorSpace c, HasOnset c, Delayable c) => a -> c -> c
group n a = repTimes n (a^/n)

groupWith :: (Enum a, Fractional a, a ~ Scalar c, Monoid c, Semigroup c, VectorSpace c, HasOnset c, Delayable c) => [a] -> c -> c
groupWith = flip $ \p -> scat . fmap (flip group $ p)

-- |
-- Reverse a score around its middle point.
--
-- > onset a    = onset (rev a)
-- > duration a = duration (rev a)
-- > offset a   = offset (rev a)
--
rev :: Score a -> Score a
rev = startAt 0 . rev'
    where
        rev' = Score . List.sortBy (comparing getT) . fmap g . getScore
        g (t,d,x) = (-(t.+^d),d,x)
        getT (t,d,x) = t

-- |
-- Repeat indefinately, like repeat for lists.
--
-- > Score Note -> Score Note
--
repeatS :: Score a -> Score a
repeatS a = a `plus` delay (duration a) (repeatS a)
    where
        Score as `plus` Score bs = Score (as <> bs)


infixl 6 ||>
a ||> b = padToBar a |> b
bar = rest^*4

padToBar a = a |> (rest ^* (d' * 4))
    where
        d  = snd $ properFraction $ duration a / 4
        d' = if (d == 0) then 0 else (1-d)


rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = (last xs:init xs)

rotated n as | n >= 0 = iterate rotr as !! n
             | n <  0 = iterate rotl as !! (abs n)

-- | 
-- @a `sampleS` b@ extracts notes from b whenever a has an occurence.
--
sampleS :: (Ord v, v ~ Voice a, HasVoice a) => Score b -> Score a -> Score (b, Score a)
sampleS x = mapVoices (fmap $ sampleSingle x)

-- | 
-- @a `sampleS` b@ filters notes from b whenever a has an occurence.
--
gateS :: Score a -> Score b -> Score b
gateS p as = mconcat $ toList $ fmap snd $ sampleSingle p as

-- | 
-- @d `takeS` a@ extracts notes occuring during duration d in a.
--
takeS :: Duration -> Score a -> Score a
takeS d = gateS (on^*d)
    where
        on  = note ()
        off = rest
        




-- |
-- Ad-hoc drawing of commands notes.
--
drawScores
    :: (Integral p, p ~ Pitch b, HasPitch b, Voice b ~ NotePart, HasVoice b)
    => Score b -> Score c -> Diagram SVG R2
drawScores notes cmds = notes1D <> notes2D <> cmdsD <> middleLines <> crossLines
    where
        notes1 = mfilter (\x -> getPartGroup (getVoice x) == 1) notes
        notes2 = mfilter (\x -> getPartGroup (getVoice x) == 2) notes

        notes1D     = mconcat $ fmap (drawNote 1) $ perform notes1
        notes2D     = mconcat $ fmap (drawNote 2) $ perform notes2
        cmdsD       = mconcat $ fmap drawCmd $ perform cmds
        middleLines = translateX ((/ 2) $ totalDur) (hrule $ totalDur)
        crossLines  = mconcat $ fmap (\n -> translateX ((totalDur/5) * n) (vrule 100)) $ [0..5]

        drawNote n (t,d,x) = translateY (getP x + off n) $ translateX (getT (t.+^(d^/2))) $ scaleX (getD d) $ noteShape n
        off 1 = 50
        off 2 = (-50)
        drawCmd (t,d,x) = translateY 0 $ translateX (getT t) $ cmdShape

        noteShape 1 = lcA transparent $ fcA (blue  `withOpacity` 0.3) $ square 1
        noteShape 2 = lcA transparent $ fcA (green `withOpacity` 0.3) $ square 1
        cmdShape = lcA (red `withOpacity` 0.3) $ vrule (200)

        totalDur = getD $ duration notes
        getT = fromRational . toRational
        getD = fromRational . toRational
        getP = (subtract 60) . fromIntegral . getPitch

reset x = moveBack (offset x .-. 0) x     