
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, OverloadedStrings #-}

module Score (
        cmdScore,
        noteScore
  ) where

import Control.Monad
import Control.Applicative
import Control.Apply.Reverse
import Control.Concurrent (threadDelay)
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Foldable (Foldable(..), toList)
import Data.Ord (comparing)
import Data.String
import Data.Ratio
import qualified Data.List as List

import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score
import Music.Score.Combinators
import Music.Score.Rhythm (quantize)

import Diagrams.Prelude hiding (open, duration, stretch, stretchTo, (|>), Time, Duration, (&), text, e, tau)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString

import Music.Imitator

--------------------------------------------------------------------------------

-- FIXME duration must be shorter than env start time
-- TODO spread out source positions (minimize risk of noise etc)

cmdScore :: Score Command
cmdScore = mempty
    <> delay 0          (readBuffer "/Users/hans/Documents/Kod/hs/music-imitator/sounds/test.aiff")
    <> delay 0          (playOnce 0 1800 & setCurve Sharp & setAzim (0.0 + 0))

    <> delay (0*60+20)  echoShort1
    -- TODO echo canon 0 ?
    <> delay (4*60+20)  echoShort2

    -- TODO
    <> delay (109*4)     echoCanon1
    <> delay (111*4)     echoCanon1

    <> delay (20*60+20+0)  echoCanon3
    <> delay (21*60+20+2)  echoCanon3
    <> delay (22*60+20+4)  echoCanon3

    -- <> delay (25*60+24+0)  echoEnd
    -- <> delay (25*60+24+2)  echoEnd

    -- <> delay (25*60+34+0)  echoEnd
    -- <> delay (25*60+34+2)  echoEnd
    -- <> delay (25*60+34+4)  echoEnd
    -- <> delay (25*60+34+6)  echoEnd

    -- <> delay (25*60+44+0)  echoEnd
    -- <> delay (25*60+44+2)  echoEnd
    -- <> delay (25*60+44+4)  echoEnd

    <> delay (duration noteScore) (note StopRecord) -- mark end

echoEnd = mempty
    |> (playOnce (25*60+4) (20*4) & setCurve Smooth & setAzim (0.0 + 0))


echoCanon1 = mempty
    |> (playOnce (107*4) (20*4) & setCurve Smooth & setAzim (0.0 + 0))

echoCanon3 = mempty
    |> (playOnce (20*60+0) (60*4) & setCurve Smooth & setAzim (0.0 + 0))

echoShort1 = mempty
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10

echoShort2 = mempty
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 10 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 50 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10



--------------------------------------------------------------------------------

-- TODO col legno in basses one octave down (do last, sounds wrong!)
-- TODO register in canons?
-- TODO replace rep with repWith, work on continous transforms
-- TODO pedals
-- TODO harmonics?


noteScore :: Score Note
noteScore = {-addInstrChange $-}

    -- part 1 (first canon and col legno)
        (colLegno1  </> delay (4*3) colLegno1) 
    ||> (canon0 <> (delay (4*5) $ moveToPart vl2 $ canon0))
    ||> (colLegno2  </> delay (4*3) colLegno2) 

    -- part 2 (canon4 and surrounding)
    ||> (bar^*30
            <> delay 0      (moveToPart vc2 g_^*(4*20))
            <> delay (4*15) (moveToPart vc1 a_^*(4*20))
            )
    ||> canon4
    ||> (bar^*15 <> moveToPart vl2 (rev canon4))
    ||> (bar^*35
            <> delay 0      (moveToPart vc2  bb_^*(4*20))
            <> delay (4*20) (moveToPart vla1 c  ^*(4*20))
            )    
    
    -- part 3 (development to canon3)
    ||> (bar^*45
            <> delay 0      (moveToPart vl1  d' ^*(4*20))
            <> delay 0      (moveToPart vla1 f' ^*(4*20))
            <> delay (4*20) (moveToPart vl2  g  ^*(4*20))
            <> delay (4*20) (moveToPart vla2 bb ^*(4*20))
            <> delay (4*40) (moveToPart vl1  bb_^*(4*20))
            <> delay (4*60) (moveToPart vla1 c  ^*(4*20))
            )
    ||> canon3
    
    -- part 4 (jete)
    ||> mconcat [
            delay 0 $ dynamics ppp $ up (12*3) $ moveToPart vl2  $ d_^*(4*30),
            delay 5 $ dynamics ppp $ up (12*3) $ moveToPart vla2 $ d_^*(4*30),
            delay (4*10) (dynamics _p $ jete1 
                </> 
            delay (12*8) jete1)
           ]
    ||> bar^*2
    ||> c'^*4 -- mark ending!  



--------------------------------------------------------------------------------

colLegno1 :: Score Note
colLegno1 = {-staccato $ -} dynamics (ppp `cresc` mp |> mp^*0.2) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repTimes 7 $ [4,4,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ repTimes 7 $ [4,4,5,4,5] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 3 $ repTimes 7 $ [4,5,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 6 $ repTimes 7 $ [3,3,5,3,5] `groupWith` g |> rest^*6)

colLegno1V :: Score Note
colLegno1V = {-staccato $ -} dynamics (ppp `cresc` mp |> mp^*0.2) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repTimes 7 $ [4,4,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ repTimes 7 $ [4,4,5,4,5] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 3 $ repTimes 7 $ [4,5,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 6 $ repTimes 7 $ [3,3,5,3,5] `groupWith` g |> rest^*6)


colLegno2 :: Score Note
colLegno2 = {-staccato $ -} dynamics (mp) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ repTimes 4 $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ repTimes 4 $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 3 $ repTimes 4 $ [4,5,4,5,4,4]  `groupWith` g |> rest^*6)
    </> (down 12 $ delay 6 $ repTimes 4 $ [3,3,5,3,3]    `groupWith` g |> rest^*6)


makeJete :: Pitch Note -> Bool -> Duration -> Score Note
makeJete p v d = text "jeté" $ modifyPitches (+ p) $ g_ |> ((if v then cs else cs_){-^/2-}) {-|> rest^/2-} |> rest^*d

makeJetes :: [Pitch Note] -> [Bool] -> [Duration] -> Score Note
makeJetes ps vs ds = scat $ zipWith3 makeJete ps vs ds

jete0 :: Score Note
jete0 = (rest <>) $ -- FIXME temporary fix w.r.t onset/padToBar 
        (delay 3  $ up 0    $ makeJetes (rotated 0 ps) (rotated 3 vs) (rotated 1 ds))
    </> (delay 5  $ up 0    $ makeJetes (rotated 1 ps) (rotated 0 vs) (rotated 3 ds))^*(4/5)
    </> (delay 7  $ down 12 $ makeJetes (rotated 2 ps) (rotated 1 vs) (rotated 2 ds))
    </> (delay 12 $ down 12 $ makeJetes (rotated 3 ps) (rotated 2 vs) (rotated 0 ds))^*(4/5)
    where
        ps = take n $ cycle [0,6,6,0,6,6,0] 
        vs = take n $ cycle [True,False,True,False,True,False,True,False]
        ds = take n $ cycle $ fmap (+ 4) [3,7,5,7,5,5,3,7,7,7,7,7,5,3,7,7,7,7,7,3,3,5]
        n  = 7

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

colLegno3 :: Score Note
colLegno3 = (down 12 $ delay 0 $ repeatS $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)


--------------------------------------------------------------------------------


makeCanon0 :: Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon0 dn subj1 subj2 = 
        dynamics dn (rev (a </> b </> c </> d) |> (a </> b </> c </> d))
    where
        a = (repTimes 5  $ {- legato $ -} subj1 ^*(4/3))
        b = (repTimes 5  $ {- legato $ -} subj2 ^*1)
        c = (repTimes 5  $ {- legato $ -} subj1 ^*2) 
        d = (repTimes 2  $ {- legato $ -} subj2 ^*3) 

canon0 :: Score Note
canon0 = text "arco" $ (^*2) $ makeCanon0 dn subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> g_^*2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*3
        dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

makeCanon4 :: Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon4 dn subj1 subj2 = 
        dynamics dn (rev $ a </> b </> c </> d)
    where
        a = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*(4/3))
        b = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*1)
        c = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*2) 
        d = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*3) 

canon4 :: Score Note
canon4 = text "arco" $ (^*2) $ makeCanon4 dn subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> c^*1 |> bb_^*1
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*3
        dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )



makeCanon2 :: Score (Levels Double) -> Score Note -> Score Note
makeCanon2 dn subj = 
        (dynamics dn $ repTimes 10 $ {- legato $ -} up   octave  $ subj ^* (2/3) )
    </> (dynamics dn $ repTimes 7  $ {- legato $ -} up   fifth   $ subj ^* 1     )
    </> (dynamics dn $ repTimes 5  $ {- legato $ -} down unison  $ subj ^* (3/2) )

canon2 :: Score Note
canon2 = down 2 $ text "arco" $ makeCanon2 dn subj
    where
        subj = (melody [d,a] |> g^*2 |> c' |> b |> c' |> b |> {-g|> a^*3-} a^*4)
        dn   = (repTimes 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)

makeCanon3 :: Bool -> Score Note -> Score Note -> Score Note -> Score Note
makeCanon3 flip subj1 subj2 bass = if flip then lower </> upper else upper </> lower
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

-- FIXME inverse dynamics
-- TODO should we just scale this up?
canon3 :: Score Note
canon3 = down 2 $ text "arco" $ c^*padC |> firstC |> secondC
    where
        firstC  = dynamics dn1 (rev (makeCanon3 False subj1 subj2 bass))
        secondC = dynamics dn2 (makeCanon3 True subj1 subj2 bass)
        padC    = fromIntegral $ 4 - numerator (getDuration $ duration firstC) `mod` 4
        dn1     = (repTimes 10 $ (mf `cresc` _f)^*5 |> (_f `dim` mf)^*5)
        dn2     = (repTimes 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)

        subj1 = (d^*3 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
        subj2 = (d^*2 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
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
main = rt

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

dr = do                                             
    let sc  = drawScores noteScore cmdScore
    let svg = renderDia SVG (SVGOptions (Dims 1800 800)) sc
    let bs  = renderSvg svg
    ByteString.writeFile "score.svg" bs

nrt = do
    writeSynthDefs
    runImitatorNRT (scoreToTrack cmdScore)

rt = do 
    startServer
    threadDelay 1000000
    runImitatorRT (scoreToTrack cmdScore)



-- TODO move all this...

-- TODO reverse score (note: do recursive reverse, for Score (Score a) etc)
-- TODO split score (note: do recursive split, for Score (Score a) etc)
-- TODO invert/retrograde etc

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


sampleS :: (Ord v, v ~ Voice a, HasVoice a) => Score b -> Score a -> Score (b, Score a)
sampleS x = mapVoices (fmap $ sampleSingle x)

gateS :: Score a -> Score b -> Score b
gateS p as = mconcat $ toList $ fmap snd $ sampleSingle p as

takeS :: Duration -> Score a -> Score a
takeS d = gateS (on^*d)

on :: Score ()
on = note ()

off :: Score ()
off = rest





