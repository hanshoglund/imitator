
{-# LANGUAGE 
    TypeFamilies, 
    DeriveFunctor, 
    DeriveFoldable, 
    GeneralizedNewtypeDeriving,
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
import Music.Imitator
import Music.Pitch.Literal
import Music.Dynamics.Literal


--------------------------------------------------------------------------------

-- FIXME duration must be shorter than env start time

cmdScore :: Score Command
cmdScore = mempty
--    |> (note $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff")
    |> (note $ ReadBuffer "/Users/hans/Documents/Kod/hs/music-imitator/sounds/canon1.aiff")
    |> sp1 |> rest^*3
    |> sp1 |> rest^*3
    |> sp1 |> rest^*5
    |> sp2 |> rest^*3
    |> sp2 |> rest^*3
    |> sp2 |> rest^*5
    -- |> sp1 |> rest^*3
    -- |> sp1 |> rest^*3
    -- |> sp1 |> rest^*5
    -- |> sp2 |> rest^*3
    -- |> sp2 |> rest^*3
    -- |> sp2 |> rest^*5

sp1 = setCurve Smooth $ mempty
    <> rest^*0.0 |> (playFromDuring 4 14 & setAzim (0.0 + 0))
    <> rest^*1.2 |> (playFromDuring 4 14 & setAzim (0.0 - 0.2))
    <> rest^*2.4 |> (playFromDuring 4 14 & setAzim (0.0 - 0.2))
    <> rest^*3.6 |> (playFromDuring 4 14 & setAzim (0.0 + 0  ))

sp2 = setCurve Smooth $ mempty
    <> rest^*0.0 |> (playFromDuring 50 14 & setAzim (0.5      ))
    <> rest^*1.2 |> (playFromDuring 50 14 & setAzim (0.5 + 0.2))
    <> rest^*2.4 |> (playFromDuring 50 14 & setAzim (0.5 - 0.2))
    <> rest^*3.6 |> (playFromDuring 50 14 & setAzim (0.5 + 0.3))
    <> rest^*4.8 |> (playFromDuring 50 14 & setAzim (0.5 - 0.3))


--------------------------------------------------------------------------------

-- TODO reorder parts?

noteScore :: Score Note
noteScore = (short1 |> rest^*4 |> canon1)
        </> (delay 4 $ down 12 $ short1 |> rest^*4 |> canon1)
    -- |> short2
    -- |> sect2
    -- |> sect3
    -- |> sect4
    -- |> sect5

short1 :: Score Note
short1 = staccato $ dynamic ppp $ text "pizz" $
        rep 30 (legato $ grp 5 c |> grp 4 db |> grp 4 c)
    </> rep 30 (legato $ grp 5 c |> grp 4 c  |> grp 4 c)
    </> rep 30 (legato $ grp 5 c |> grp 5 c  |> grp 4 db)
    </> rep 30 (legato $ grp 5 c |> grp 5 c  |> grp 5 c |> grp 4 b_)
    where
        grp n p = rep n p^/n

short2 :: Score Note
short2 = 
    short1
    </> 
    (rep 2 $ delay 30 $ rep 2 $ modifyPitches (+ 3) $ short1)
    -- </> (rep 1 $ delay 55 $ rep 2 $ modifyPitches (+ 6) $ short1)


text s = mapSep (setText s) id id

-- sect1 :: Score Note
-- sect1 = (^*2) $ dynamic _f $ mempty
--     <>  (melody [c,d] |> f^*(3/2) |> e & legato & rep (10) & modifyPitches (+ 12))          ^*(4/3)
--     </> (melody [c,d]                  & legato & rep (15) & modifyPitches (+ 5))           ^*1
--     </> (melody [c,d] |> f^*(3/2) |> e & legato & rep (20))                                 ^*2
--     </> (melody [c,d]                  & legato & rep (10) & modifyPitches (subtract 12))   ^*3
-- 
-- sect2 = sect1
-- sect3 = sect1
-- sect4 = sect1
-- sect5 = sect1


makeCanon1 :: Score (Dyn Double) -> Score Note -> Score Note
makeCanon1 dn subj = 
        (dyn dn $ rep 100 $ legato $ up 7 $ subj ^* (2/3) )
    </> (dyn dn $ rep 100 $ legato $ up 7 $ subj ^* 1     ) 
    </> (dyn dn $ rep 100 $ legato $ up 0 $ subj ^* (3/2) ) 
    </> (dyn dn $ rep 100 $ legato $ up 0 $ subj ^* 2     ) 

canon1 :: Score Note
canon1 = up 12 $ text "arco" $ makeCanon1 dn subj
    where
        subj = (e^*2 |> melody [e,f,e,c] |> d^*4)^/1
        dn   = (rep 10 $ (cresc ppp mf)^*3 |> mp |> (dim mp ppp)^*3 |> ppp )




--------------------------------------------------------------------------------

main :: IO ()
main = rt

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



--------------------------------------------------------------------------------
-- Pitch
--------------------------------------------------------------------------------

-- TODO better transposition etc
-- TODO interval literals (Music.Pitch.Interval.Literal)
up x = fmap (modifyPitch (+ x))
down x = fmap (modifyPitch (subtract x))

-- TODO move to Music.Pitch.Interval.Literal
octave     = 12
tritone    = 6
fifth      = 7
minorThird = 3
majorThird = 4


--------------------------------------------------------------------------------
-- Structure
--------------------------------------------------------------------------------

rep 0 x = mempty
rep n x = x |> rep (n-1) x


-- TODO reverse score (note: do recursive reverse, for Score (Score a) etc)
-- TODO split score (note: do recursive split, for Score (Score a) etc)
-- TODO invert/retrograde etc







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

type Note = (VoiceT NotePart (TieT (TremoloT (DynamicT (ArticulationT (TextT Integer))))))

score x = (x::Score Note)

open :: Score Note -> IO ()
open = openXml . (^/4)            

play :: Score Note -> IO ()
play = playMidiIO            


data NotePart = Vl1 | Vl2 | Vla1 | Vla2 | Vc1 | Vc2 | Db1 | Db2
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
    show Db1  = "Contrabass 1"
    show Db2  = "Contrabass 2"

vl1, vl2, vla1, vla2, vc1, vc2, db1, db2 :: NotePart
vl1  = Vl1
vl2  = Vl2
vla1 = Vla1
vla2 = Vla2
vc1  = Vc1
vc2  = Vc2
db1  = Db1
db2  = Db2






{-
-- FIXME
vzip xs = Prelude.foldr1 (</>) $ fmap (setVoices $ toEnum 0) $ (odds $ voices xs) ++ (evens $ voices xs)
    where                     
        
        odds []  = []
        odds [x] = [x]
        odds (a:_:as) = a:(odds as)
        evens []  = []
        evens [_] = []
        evens (_:a:as) = a:(evens as)

zipVoices xs ys = Prelude.foldr1 (</>) $ zipWith (</>) xs ys

-}




-- Voice catenation

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
toPart :: (Enum v, v ~ Voice a, Functor s, HasVoice a) => v -> s a -> s a
toPart v = moveParts (fromEnum v)

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
mapSep f g h = mapVoices (fmap $ mapSepPart f g h)

mapSepPart :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepPart f g h = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform
    where                
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)


instance IsPitch Integer where
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
