
{-# LANGUAGE 
    TypeFamilies, 
    DeriveFunctor, 
    DeriveFoldable, 
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction,
    OverloadedStrings #-}

module Score (
        cmdScore
  ) where

import Data.Foldable (Foldable(..), toList)
import Math.Tau
import Data.String
import Data.Ratio
import qualified Data.List as List
import Data.Semigroup
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Apply.Reverse
import Data.VectorSpace
import Data.AffineSpace
import Music.Score
import Music.Score.Rhythm (quantize)
import Music.Imitator
import Music.Pitch.Literal
import Music.Dynamics.Literal

import Diagrams.Prelude hiding (open, duration, stretch, Time, (|>), Duration, (&), text, e, tau)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Diagrams.Backend.SVG.CmdLine
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as ByteString

quantizeScore = fmap (quantize . getPart) . scoreToParts

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
        (short1 </> delay (4*1) short1) 
    ||> (canon00 <> (delay (4*5) $ moveToPart vl2 $ canon00))
    ||> (short1 </> delay (4*3) short1) 

    ||> c'^*4
    ||> (canon0 <> (delay (4*5) $ moveToPart vl2 $ canon0))
    ||> c'^*4

    -- ||> rest^*(4*(15))
    -- ||> (canon1 <> (delay (4*7) $ moveToPart vl2 $ canon1))

    -- ||> rest^*(4*(40))
    -- ||> (canon1 <> (delay (4*7) $ moveToPart vl2 $ canon1))
    -- ||> rest^*7 
    -- 
    -- ||> rest^*(4*(25))
    -- ||> ((delay (4*5) $ canon2) <> (moveToPart vl2 $ down octave $ canon2))
    -- 
    -- ||> rest^*(4*(40))
    -- ||> ((delay (4*5) $ canon3) <> (moveToPart vl2 $ canon3))
    -- 
    -- ||> rest^*(4*(90))     
    -- ||> c'^*4 -- mark ending!  

infixl 6 ||>
a ||> b = padToBar a |> b

padToBar a = a |> (rest ^* (d' * 4))
    where
        d  = snd $ properFraction $ duration a / 4
        d' = if (d == 0) then 0 else (1-d)


short1 :: Score Note
short1 = staccato $ dynamic ppp $ text "col legno battuto"  $
        (down 12 $ delay 0 $ rep 7 $ legato $ g `withGroups` [4,4,4,5,4] |> rest^*6)
    </> (down 12 $ delay 1 $ rep 7 $ legato $ g `withGroups` [4,4,5,4,5] |> rest^*6)
    </> (down 12 $ delay 3 $ rep 7 $ legato $ g `withGroups` [4,5,4,5,4] |> rest^*6)
    </> (down 12 $ delay 6 $ rep 7 $ legato $ g `withGroups` [3,3,5,3,5] |> rest^*6)
    where
        withGroups p = scat . fmap (\d -> group d p)




makeCanon0 :: Score (Dyn Double) -> Score Note -> Score Note -> Score Note
makeCanon0 dn subj1 subj2 = 
        (dyn dn $ repeated $ legato $ subj1 ^*(4/3))
    </> (dyn dn $ repeated $ legato $ subj2 ^*1)
    </> (dyn dn $ repeated $ legato $ subj1 ^*2) 

makeCanon1 :: Score (Dyn Double) -> Score Note -> Score Note
makeCanon1 dn subj = 
        (dyn dn $ repeated $ legato $ up   fifth  $ subj ^* (2/3) )
    </> (dyn dn $ repeated $ legato $ up   fifth  $ subj ^* 1     )
    </> (dyn dn $ repeated $ legato $ down unison $ subj ^* (3/2) )

makeCanon2 :: Score (Dyn Double) -> Score Note -> Score Note
makeCanon2 dn subj = 
        (dyn dn $ repeated $ legato $ up   octave  $ subj ^* (2/3) )
    </> (dyn dn $ repeated $ legato $ up   fifth   $ subj ^* 1     )
    </> (dyn dn $ repeated $ legato $ down unison  $ subj ^* (3/2) )

makeCanon3 :: Score (Dyn Double) -> Score Note -> Score Note
makeCanon3 dn subj = 
        (dyn dn $ repeated $ legato $ up   (octave+fifth)  $ subj ^* (2/3) )
    </> (dyn dn $ repeated $ legato $ up   octave          $ subj ^* 1     )
    </> (dyn dn $ repeated $ legato $ up   fifth           $ subj ^* (3/2) )
    </> (dyn dn $ repeated $ legato $ down octave          $ subj ^* (3/2) )


canon00 :: Score Note
canon00 = text "arco" $ (^*2) $ makeCanon0 dn subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> g_^*2 |> a_
        subj2 = g_^*3 |> a_ |> bb_^*2 |> c^*2
        dn   = (rep 5 $ (pp `cresc` mf)^*3 |> mf |> (mf `dim` pp)^*3 |> pp )

canon0 :: Score Note
canon0 = text "arco" $ (^*2) $ makeCanon0 dn subj1 subj2
    where
        subj1 = melody [g_,a_] |> d^*(3/2) |> c |> d
        subj2 = g_^*3 |> a_ |> bb_^*2 |> c^*2
        dn   = (rep 5 $ (pp `cresc` mf)^*3 |> mf |> (mf `dim` pp)^*3 |> pp )

canon1 :: Score Note
canon1 = down 2 $ text "arco" $ makeCanon1 dn subj
    where
        subj = (f^*2 |> melody [e,f,e,c] |> d^*4)
        dn   = (rep 13 $ (pp `cresc` mf)^*3 |> mf |> (mf `dim` pp)^*3 |> pp )

canon2 :: Score Note
canon2 = down 2 $ text "arco" $ makeCanon2 dn subj
    where
        subj = (melody [d,a] |> g^*2 |> c' |> b |> c' |> b |> {-g|> a^*3-} a^*4)
        dn   = (rep 10 $ (_p `cresc` _f)^*5 |> _f |> (_f `dim` _p)^*5 |> _p )

canon3 :: Score Note
canon3 = down 2 $ text "arco" $ makeCanon3 dn subj
    where
        subj = (melody [d,a] |> g^*2 |> c' |> b |> c' |> b |> {-g|> a^*3-} a^*4)
        dn   = (rep 10 $ (_f `cresc` ff)^*5 |> ff |> (ff `dim` f)^*5 |> _f )




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

type Note = (VoiceT NotePart (TieT (TremoloT (DynamicT (ArticulationT (TextT Integer))))))

score x = (x::Score Note)

open :: Score Note -> IO ()
open = openXml . (^/4)            

play :: Score Note -> IO ()
play = playMidiIO            

--------------------------------------------------------------------------------

main :: IO ()
main = dr


drawScores :: (Integral p, p ~ Pitch b, HasPitch b, Voice b ~ NotePart, HasVoice b) => Score b -> Score c -> Diagram SVG R2
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

        
        noteShape 1 = lcA transparent $ fcA (blue  `withOpacity` 0.3) $ circle 1
        noteShape 2 = lcA transparent $ fcA (green `withOpacity` 0.3) $ circle 1
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














--------------------------------------------------------------------------------
-- Articulation
--------------------------------------------------------------------------------

-- Accents

accent :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
accent = mapSep (setAccLevel 1) id id

marcato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
marcato = mapSep (setAccLevel 2) id id

accentLast :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
accentLast = mapSep id id (setAccLevel 1)

marcatoLast :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
marcatoLast = mapSep id id (setAccLevel 2)

-- Phrasing

tenuto :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
tenuto = mapSep (setStaccLevel (-2)) (setStaccLevel (-2)) (setStaccLevel (-2)) 

separated :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
separated = mapSep (setStaccLevel (-1)) (setStaccLevel (-1)) (setStaccLevel (-1)) 

staccato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
staccato = mapSep (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

portato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
portato = staccato . legato 

legato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
legato = mapSep (setBeginSlur True) id (setEndSlur True) 

spiccato :: (HasArticulation a, HasVoice a, Ord v, v ~ Voice a) => Score a -> Score a
spiccato = mapSep (setStaccLevel 2) (setStaccLevel 2) (setStaccLevel 2) 


--------------------------------------------------------------------------------
-- Dynamics
--------------------------------------------------------------------------------

-- | Apply a constant level over the whole score.
dynamic :: (HasDynamic a, HasVoice a, Ord v, v ~ Voice a) => Double -> Score a -> Score a
dynamic n = mapSep (setLevel n) id id 

-- | Apply a variable level over the score.
dyn :: HasDynamic a => Score (Dyn Double) -> Score a -> Score a
dyn ds = applyDynSingle (fmap fromJust . scoreToPart $ ds)

applyDyn :: (Ord v, v ~ Voice a, HasVoice a, HasDynamic a) => Part (Dyn Double) -> Score a -> Score a
applyDyn ds = mapVoices (fmap $ applyDynSingle ds)

-- Dynamic action over a duration
data Dyn a
    = Level  a
    | Change a a

instance Fractional a => IsDynamics (Dyn a) where
    fromDynamics (DynamicsL (Just a, Nothing)) = Level (toFrac a)
    fromDynamics (DynamicsL (Just a, Just b))  = Change (toFrac a) (toFrac b)
    fromDynamics x = error $ "fromDynamics: Invalid dynamics literal " {- ++ show x-}

cresc :: IsDynamics a => Double -> Double -> a
cresc a b = fromDynamics $ DynamicsL ((Just a), (Just b))

dim :: IsDynamics a => Double -> Double -> a
dim a b = fromDynamics $ DynamicsL ((Just a), (Just b))


-- end cresc, end dim, level, begin cresc, begin dim
type Dyn2 a = (Bool, Bool, Maybe a, Bool, Bool)

dyn2 :: Ord a => [Dyn a] -> [Dyn2 a]
dyn2 = snd . List.mapAccumL g (Nothing, False, False) -- level, cresc, dim
    where
        g (Nothing, False, False) (Level b)     = ((Just b,  False, False), (False, False, Just b,  False, False))
        g (Nothing, False, False) (Change b c)  = ((Just b,  b < c, b > c), (False, False, Just b,  b < c, b > c))

        g (Just a , cr, dm) (Level b) 
            | a == b                            = ((Just b,  False, False), (cr,    dm,    Nothing, False, False))
            | a /= b                            = ((Just b,  False, False), (cr,    dm,    Just b,  False, False))
        g (Just a , cr, dm) (Change b c) 
            | a == b                            = ((Just b,  b < c, b > c), (cr,    dm,    Nothing, b < c, b > c))
            | a /= b                            = ((Just b,  False, False), (cr,    dm,    Just b,  b < c, b > c))



transf :: ([a] -> [b]) -> Part a -> Part b
transf f = Part . uncurry zip . second f . unzip . getPart

applyDynSingle :: HasDynamic a => Part (Dyn Double) -> Score a -> Score a
applyDynSingle ds as = applySingle' ds3 as
    where
        -- ds2 :: Part (Dyn2 Double)
        ds2 = transf dyn2 ds
        -- ds3 :: Part (Score a -> Score a)
        ds3 = (flip fmap) ds2 g
        
        g (ec,ed,l,bc,bd) = id
                . (if ec then map1 (setEndCresc     True) else id)
                . (if ed then map1 (setEndDim       True) else id)
                . (if bc then map1 (setBeginCresc   True) else id)
                . (if bd then map1 (setBeginDim     True) else id)
                . (maybe id (\x -> map1 (setLevel x)) $ l)
        map1 f = mapSepPart f id id


-- applySingle :: Part (a -> b) -> Score a -> Score b
-- applySingle fs = applySingle' (fmap fmap fs)

-- FIXME work with infinite parts
applySingle' :: Part (Score a -> Score b) -> Score a -> Score b
applySingle' fs as = notJoin $ fmap (\(f,s) -> f s) $ sampled
    where            
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin = mconcat . toList
        sampled = sampleSingle (partToScore fs) as

-- |
-- Get all notes that start during a given note.
--
sampleSingle :: Score a -> Score b -> Score (a, Score b)
sampleSingle as bs = Score . fmap (\(t,d,a) -> (t,d,g a (onsetIn t d bs))) . getScore $ as
    where
        g Nothing  z = Nothing
        g (Just a) z = Just (a,z)


-- | Filter out events that has its onset in the given time interval (inclusive start).
--   For example, onset in 1 2 filters events such that (1 <= onset x < 3)
onsetIn :: Time -> Duration -> Score a -> Score a
onsetIn a b = Score . mfilter (\(t,d,x) -> a <= t && t < a .+^ b) . getScore 


--------------------------------------------------------------------------------
-- Ornaments etc
--------------------------------------------------------------------------------

tremolo :: (Functor f, HasTremolo b) => Int -> f b -> f b
tremolo n = fmap (setTrem n)

text :: (Ord v, v ~ Voice b, HasVoice b, HasText b) => String -> Score b -> Score b
text s = mapSep (addText s) id id


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
rep :: (Monoid a, Semigroup a, HasOnset a, Delayable a) => Duration -> a -> a
rep 0 a = mempty
rep n a = a |> rep (n-1) a

-- | 
-- Repeat once for each element in the list.
-- 
-- > [a] -> (a -> Score Note) -> Score Note
--     
-- Example:
-- > repWith [1,2,1] (c^*)
--
repWith :: (Monoid c, HasOnset c, Delayable c) => [a] -> (a -> c) -> c
repWith = flip scatMap

scatMap f = scat . fmap f
        
    
-- | 
-- Repeat exact amount of times with an index.
-- 
-- > Duration -> (Duration -> Score Note) -> Score Note
--
repWithIndex :: (Monoid c, HasOnset c, Delayable c) => Duration -> (Duration -> c) -> c
repWithIndex n = repWith [0..n-1]

-- | 
-- Repeat exact amount of times with relative time.
-- 
-- > Real a => a -> (Time -> Score Note) -> Score Note
--
repWithTime n = repWith $ fmap (/ n') [0..(n' - 1)]
    where
        n' = Time $ toRational n

-- | 
-- Repeat a number of times and scale down by the same amount.
-- 
group n a = rep n (a^/n)

-- |
-- Repeat indefinately, like repeat for lists.
--
-- > Score Note -> Score Note
--
repeated :: (Monoid a, Semigroup a, HasOnset a, Delayable a) => a -> a
repeated = rep 50 
-- FIXME should be 
-- repeated a = a |> repeated 



-- TODO reverse score (note: do recursive reverse, for Score (Score a) etc)
-- TODO split score (note: do recursive split, for Score (Score a) etc)
-- TODO invert/retrograde etc
                                                   
--------------------------------------------------------------------------------
-- Voice composition
--------------------------------------------------------------------------------

infixr 6 </>

-- |
-- Similar to '<>', but increases voices in the second part to prevent voice collision.
--
(</>) :: (Enum v, Ord v, v ~ Voice a, Alternative s, Foldable s, HasVoice a) => s a -> s a -> s a
a </> b = a <|> moveParts offset b
    where               
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ getVoices a


-- |
-- Move down one voice (all parts).
--
moveParts :: (Enum v, v ~ Voice a, Integral b, Functor s, HasVoice a) => b -> s a -> s a
moveParts x = modifyVoices (successor x)

-- |
-- Move top-part to the specific voice (other parts follow).
--
moveToPart :: (Enum v, v ~ Voice a, Functor s, HasVoice a) => v -> s a -> s a
moveToPart v = moveParts (fromEnum v)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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
