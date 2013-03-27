
{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Score (
        cmdScore
  ) where

-- import Control.Apply.Reverse
import Data.Foldable (Foldable(..))
import Data.String
import Data.Ratio
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
--    |> (note $ ReadBuffer "/Users/hans/Desktop/Test/Test1loud.aiff")
    |> (note $ ReadBuffer "/Users/hans/Documents/Kod/hs/music-imitator/sounds/short2.aiff")
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



noteScore :: Score Note
noteScore = mempty
    |> (sect1 </> modifyPitches (subtract 12) sect1^*2)
    -- |> sect2
    -- |> sect3
    -- |> sect4
    -- |> sect5

short2 :: Score Note
short2 = 
    short1
    </> 
    (rep 2 $ delay 30 $ rep 2 $ modifyPitches (+ 3) $ short1)
    -- </> (rep 1 $ delay 55 $ rep 2 $ modifyPitches (+ 6) $ short1)


short1 :: Score Note
short1 = staccato $ dyn ppp $ mempty
    <>  rep 30 (legato $ grp 5 c |> grp 4 db |> grp 3 c)
    </> rep 30 (legato $ grp 5 c |> grp 4 c  |> grp 4 c)
    </> rep 30 (legato $ grp 5 c |> grp 5 c  |> grp 4 db)
    </> rep 30 (legato $ grp 5 c |> grp 5 c  |> grp 5 c |> grp 4 b_)
    where
        grp n p = rep n p^/n


sect1 :: Score Note
sect1 = (^*2) $ dyn _f $ mempty
    <>  (melody [c,d] |> f^*(3/2) |> e & legato & rep (10) & modifyPitches (+ 12))          ^*(4/3)
    </> (melody [c,d]                  & legato & rep (15) & modifyPitches (+ 5))           ^*1
    </> (melody [c,d] |> f^*(3/2) |> e & legato & rep (20))                                 ^*2
    </> (melody [c,d]                  & legato & rep (10) & modifyPitches (subtract 12))   ^*3

sect2 = sect1
sect3 = sect1
sect4 = sect1
sect5 = sect1


-- To each voice add slur from first to last note
-- slur :: Score Note -> Score Note

acc :: (HasArticulation a, HasVoice a, Eq v, v ~ Voice a) => Score a -> Score a
acc = mapSep (setAccLevel 1) (setAccLevel 0) (setAccLevel 0)

staccato :: (HasArticulation a, HasVoice a, Eq v, v ~ Voice a) => Score a -> Score a
staccato = mapSep (setStaccLevel 1) (setStaccLevel 1) (setStaccLevel 1) 

legato :: (HasArticulation a, HasVoice a, Eq v, v ~ Voice a) => Score a -> Score a
legato = mapSep (setBeginSlur True) id (setEndSlur True) 

portato :: (HasArticulation a, HasVoice a, Eq v, v ~ Voice a) => Score a -> Score a
portato = mapSep (setBeginSlur True . setStaccLevel 1) (setStaccLevel 1) (setEndSlur True . setStaccLevel 1) 

dyn :: (HasDynamic a, HasVoice a, Eq v, v ~ Voice a) => Double -> Score a -> Score a
dyn n = mapSep (setLevel n) id id

-- trem :: Int -> Score a -> Score a
trem :: (Functor f, HasTremolo b) => Int -> f b -> f b
trem n = fmap (setTrem n)


-- modifyPitch 


























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

-- type Diagram = ()
-- cmdsToSvg :: Track Command -> Diagram
-- cmdsToSvg = undefined

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

type Note = (VoiceT NotePart (TieT (TremoloT (DynamicT (ArticulationT Double)))))

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


-- Voice catenation

infixr 6 </>

vzip xs = Prelude.foldr1 (</>) $ fmap (setVoices $ toEnum 0) $ (odds $ voices xs) ++ (evens $ voices xs)
    where                     
        
        odds []  = []
        odds [x] = [x]
        odds (a:_:as) = a:(odds as)
        evens []  = []
        evens [_] = []
        evens (_:a:as) = a:(evens as)

zipVoices xs ys = Prelude.foldr1 (</>) $ zipWith (</>) xs ys

-- |
-- Similar to '<>', but offsets voices in the second part to prevent voice collision.
--
(</>) :: (Enum v, Eq v, v ~ Voice a, Functor s, Foldable s, Semigroup (s a), HasVoice a) => s a -> s a -> s a
a </> b = a <> modifyVoices (successor offset) b
    where
        offset = succ $ maximum $ fmap fromEnum $ (getVoices a ++ [toEnum 0])

-- |
-- Move down one voice (all parts).
--
moveDown :: (Enum v, v ~ Voice a, Integral b, Functor s, HasVoice a) => b -> s a -> s a
moveDown x = modifyVoices (successor x)

-- |
-- Move top-parrt to the specific voice (other parts follow).
--
moveTo :: (Enum v, v ~ Voice a, Functor s, HasVoice a) => v -> s a -> s a
moveTo v = moveDown (fromEnum v)

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

infixl 1 &
(&) = flip ($)


-- | Map over first, middle and last elements of list.
--   Biased on first, then on first and last for short lists.
mapSepL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapSepL f g h []      = []
mapSepL f g h [a]     = [f a]
mapSepL f g h [a,b]   = [f a, h b]
mapSepL f g h xs      = [f $ head xs] ++ (map g $ tail $ init xs) ++ [h $ last xs]

mapSep :: (HasVoice a, Eq v, v ~ Voice a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSep f g h = mapVoices (fmap $ mapSepPart f g h)

mapSepPart :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapSepPart f g h = mconcat . mapSepL (fmap f) (fmap g) (fmap h) . fmap toSc . perform
    where                
        third f (a,b,c) = (a,b,f c)
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
