
{-# LANGUAGE 
    TypeFamilies, 
    DeriveFunctor, 
    DeriveFoldable, 
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction,
    OverloadedStrings #-}

module Score (
        cmdScore,
        noteScore
  ) where

import Control.Monad
import Control.Applicative
import Control.Apply.Reverse
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Foldable (Foldable(..), toList)
import Data.String
import Data.Ratio
import Data.Ord (comparing)
import qualified Data.List as List

import Control.Concurrent (threadDelay)

import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score
import Music.Score.Rhythm (quantize)

import Diagrams.Prelude hiding (open, duration, stretch, stretchTo, (|>), 
                                Time, Duration, 
                                (&), text, e, tau)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString

import Music.Imitator

quantizeScore = fmap (quantize . getPart) . scoreToParts . (^/4)

--------------------------------------------------------------------------------

-- FIXME duration must be shorter than env start time

cmdScore :: Score Command
cmdScore = mempty
    <> delay 0          (readBuffer "/Users/hans/Documents/Kod/hs/music-imitator/sounds/test.aiff")
    <> delay 0          (playOnce 0 1800 & setCurve Sharp & setAzim (0.0 + 0))

    <> delay (0 *60+20)  echoShort1
    <> delay (2 *60+20)  echoShort2

    -- TODO
    <> delay (109*4)     echoCanon1
    <> delay (111*4)     echoCanon1

    <> delay (319*4)     echoCanon3
    <> delay (322*4)     echoCanon3
    <> delay (324*4)     echoCanon3
    

    <> delay (duration noteScore) (note StopRecord) -- mark end

echoCanon1 = mempty
    |> (playOnce (107*4) (20*4) & setCurve Smooth & setAzim (0.0 + 0))

echoCanon3 = mempty
    |> (playOnce (317*4) (20*4) & setCurve Smooth & setAzim (0.0 + 0))

echoShort1 = mempty
    |> (playOnce 10 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 10 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10

echoShort2 = mempty
    |> (playOnce 10 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 10 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 15 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10
    |> (playOnce 20 180 & setCurve Smooth & setAzim (0.0 + 0))
    |> rest^*10



--------------------------------------------------------------------------------

-- TODO reorder parts?
-- TODO harmonics (nat + art)  
-- TODO remove all slurs!!!!

noteScore :: Score Note
noteScore = addInstrChange $
    -- part 1
        (short1  </> delay (4*3) short1) 
    ||> (canon00 <> (delay (4*5) $ moveToPart vl2 $ canon00))

    ||> (short1  </> delay (4*3) short1) 
    ||> bar^*30
    
    -- part 2
    ||> bar^*15
    ||> (canon1  <> (delay (4*7) $ moveToPart vl2 $ canon1))
    
    ||> bar^*40
    ||> (canon1  <> (delay (4*7) $ moveToPart vl2 $ canon1))
    
    -- part 3
    ||> bar^*25
    ||> ((delay (4*5) $ canon2) <> (moveToPart vl2 $ down octave $ canon2))
    
    ||> bar^*10
    ||> (rev $ (delay (4*1) $ canon3) <> (moveToPart vl2 $ canon3))
    ||> ((delay (4*1) $ canon3) <> (moveToPart vl2 $ canon3))
    
    -- part 4 (coda)
    ||> bar^*15           
    ||> (jete1 </> delay (12*8) jete1)
    ||> bar^*15     
    ||> c'^*4 -- mark ending!  


-- TODO replace rep with repWith, work on continous transforms
-- TODO pedals
-- TODO harmonics?

short1 :: Score Note
short1 = {-staccato $ -} dynamics (ppp `cresc` mp |> mp^*0.2) $ text "col legno battuto"  $
        (down 12 $ delay 0 $ rep 7 $ [4,4,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 1 $ rep 7 $ [4,4,5,4,5] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 3 $ rep 7 $ [4,5,4,5,4] `groupWith` g |> rest^*6)
    </> (down 12 $ delay 6 $ rep 7 $ [3,3,5,3,5] `groupWith` g |> rest^*6)
    where


makeJete :: Pitch Note -> Bool -> Duration -> Score Note
makeJete p v d = text "jeté" $ modifyPitches (+ p) $ g_ |> ((if v then cs else cs_){-^/2-}) {-|> rest^/2-} |> rest^*d

makeJetes :: [Pitch Note] -> [Bool] -> [Duration] -> Score Note
makeJetes ps vs ds = scat $ zipWith3 makeJete ps vs ds

jete1 :: Score Note
jete1 = (rest <>) $ -- FIXME temporary fix w.r.t onset/padToBar 
        (delay 3  $ up 0    $ makeJetes (rotated 0 ps) (rotated 3 vs) (rotated 1 ds))
    </> (delay 5  $ up 0    $ makeJetes (rotated 1 ps) (rotated 0 vs) (rotated 3 ds))^*(4/5)
    </> (delay 7  $ down 12 $ makeJetes (rotated 2 ps) (rotated 1 vs) (rotated 2 ds))
    </> (delay 12 $ down 12 $ makeJetes (rotated 3 ps) (rotated 2 vs) (rotated 0 ds))^*(4/5)
    where
        ps = take n $ cycle [0,6,6,0,6,6,0] 
        vs = take n $ cycle [True,False,True,False,True,False,True,False]
        ds = take n $ cycle $ fmap (+ 4) [3,7,5,7,5,5,3,7,7,7,7,7,5,3,7,7,7,7,7,3,3,5]
        n  = 9

makeCanon0 :: Score (Levels Double) -> Score Note -> Score Note -> Score Note
makeCanon0 dn subj1 subj2 = 
        dynamics dn (rev (a </> b </> c </> d) |> (a </> b </> c </> d))
    where
        a = (rep 5  $ {- legato $ -} subj1 ^*(4/3))
        b = (rep 4  $ {- legato $ -} subj2 ^*1)
        c = (rep 3  $ {- legato $ -} subj1 ^*2) 
        d = (rep 2  $ {- legato $ -} subj2 ^*3) 

makeCanon1 :: Score (Levels Double) -> Score Note -> Score Note
makeCanon1 dn subj = 
        (dynamics dn $ rep 10 $ {- legato $ -} up   fifth  $ subj ^* (2/3) )
    </> (dynamics dn $ rep 7  $ {- legato $ -} up   fifth  $ subj ^* 1     )
    </> (dynamics dn $ rep 5  $ {- legato $ -} down unison $ subj ^* (3/2) )

makeCanon2 :: Score (Levels Double) -> Score Note -> Score Note
makeCanon2 dn subj = 
        (dynamics dn $ rep 10 $ {- legato $ -} up   octave  $ subj ^* (2/3) )
    </> (dynamics dn $ rep 7  $ {- legato $ -} up   fifth   $ subj ^* 1     )
    </> (dynamics dn $ rep 5  $ {- legato $ -} down unison  $ subj ^* (3/2) )

makeCanon3 :: Score (Levels Double) -> Score Note -> Score Note
makeCanon3 dn subj = 
        (dynamics dn $ rep 10 $ {- legato $ -} up   (octave+fifth)  $ subj ^* (2/3) )
    </> (dynamics dn $ rep 10 $ {- legato $ -} up   octave          $ subj ^* 1     )
    </> (dynamics dn $ rep 7  $ {- legato $ -} up   fifth           $ subj ^* (3/2) )
    </> (dynamics dn $ rep 5  $ {- legato $ -} down octave          $ subj ^* 2 )


canon00 :: Score Note
canon00 = text "arco" $ (^*2) $ makeCanon0 dn subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> g_^*2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*3
        dn   = (rep 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

canon0 :: Score Note
canon0 = text "arco" $ (^*2) $ makeCanon0 dn subj1 subj2
    where
        subj1 = melody [g_,a_] |> d^*(3/2) |> c |> d
        subj2 = g_^*3 |> a_ |> bb_^*2 |> c^*2
        dn   = (rep 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

canon1 :: Score Note
canon1 = down 2 $ text "arco" $ makeCanon1 dn subj
    where
        subj = (f^*2 |> melody [e,f,e,c] |> d^*4)
        dn   = (rep 13 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

canon2 :: Score Note
canon2 = down 2 $ text "arco" $ makeCanon2 dn subj
    where
        subj = (melody [d,a] |> g^*2 |> c' |> b |> c' |> b |> {-g|> a^*3-} a^*4)
        dn   = (rep 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)

canon3 :: Score Note
canon3 = down 2 $ text "arco" $ makeCanon3 dn subj
    where
        subj = (melody [d,a] |> g^*2 |> c' |> b |> c' |> b |> {-g|> a^*3-} a^*4)
        dn   = (rep 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)




--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

instance Monoid Command where
    mempty = PlayBuffer 0 0 1 0.5 Standard 0
    x `mappend` y = x

readBuffer = note . ReadBuffer
playOnce t d = setTime t $ setDur d $ note mempty

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

        drawNote n (t,d,x) = translateY (getP x + off n) $ translateX (getT t) $ scaleX (getD d) $ noteShape n
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
rep :: (Enum a, Monoid c, HasOnset c, Delayable c) => a -> c -> c
rep n a = replicate (0 `max` fromEnum n) () `repWith` (const a)

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
group n a = rep n (a^/n)

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
repeated :: (Monoid c, HasOnset c, Delayable c) => c -> c
repeated = rep 500
-- TODO proper impl

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


--------------------------------------------------------------------------------


{-
successor :: (Integral b, Enum a) => b -> a -> a
successor n | n <  0 = (!! fromIntegral (abs n)) . iterate pred
            | n >= 0 = (!! fromIntegral n)       . iterate succ

-- | 
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
-- 
mapSepL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapSepL f g h []      = []
mapSepL f g h [a]     = [f a]
mapSepL f g h [a,b]   = [f a, h b]
mapSepL f g h xs      = [f $ head xs] ++ (map g $ tail $ init xs) ++ [h $ last xs]

mapSep :: (HasVoice a, Ord v, v ~ Voice a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSep f g h sc = fixDur . mapVoices (fmap $ mapSepPart f g h) $ sc
    where
        fixDur a = padAfter (duration sc - duration a) a


mapSepPart :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepPart f g h sc = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform $ sc
    where             
        fixDur a = padAfter (duration sc - duration a) a
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)

padAfter :: Duration -> Score a -> Score a
padAfter d a = a |> (rest^*d)

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)

-- | 
-- Pass through @Just@ occurrences.
-- Generalizes the 'catMaybes' function.
-- 
mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= maybe mzero return)

second :: (a -> b) -> (c,a) -> (c,b)
second f (a,b) = (a,f b)

partToScore' :: Part (Maybe a) -> Score a
partToScore' = mcatMaybes . partToScore

-- mapParts :: (Ord v, v ~ Voice a, HasVoice a) => (Part (Maybe a) -> Part (Maybe a)) -> Score a -> Score a
-- mapParts f = mapVoices (fmap $ mcatMaybes . partToScore . f . scoreToPart)

toFrac :: (Real a, Fractional b) => a -> b
toFrac = fromRational . toRational

fromJust (Just x) = x
-}
