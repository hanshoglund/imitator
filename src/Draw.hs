
module Draw where

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





-- -- |
-- -- Ad-hoc drawing of commands notes.
-- --
-- drawPoster :: Diagram SVG R2
-- drawPoster = mconcat $ fmap (\(i,a) -> translate (rotate (tau*a::Rad) $ r2 (0,z)) $ alignB $ rotate (tau*a::Rad) $ alignX 0 $ alignY 0 $ notes i) $ zip [0,1..] $ [0,(1/n)..1]
--     where     
--         notes i = alignTR $ mconcat $ fmap (drawNote 1) $ perform $ ns (i `mod` 4)
--                                                                                
--         
--         n  = 8
--         z  = 100
--         dr = 40
--         ns v = (takeS dr $ (!! v) $ voices $ rev canon_IV)
-- 
--         drawNote n (t,d,x) =
--                 drawNote' n (t,d,x)
--                  <> drawFigure n (t,d,x)
--         
--         drawFigure n (t,d,x) = id
--                 $ translateY (getP x-30) 
--                 $ translateX (getT (t.+^(d)))  
--                 $ lw 0.2
--                 $ alignTL
--                 $ lcA transparent
--                 $ fcA (black `withOpacity` 0.2) 
--                 -- $ vrule (fromIntegral $ 2 * (round $ getTime $ t) `mod` 4)
--                 $ rect 0.4 (fromIntegral $ 2 * (round $ getTime $ t) `mod` 4)
-- 
-- 
--         drawNote' n (t,d,x) = id
--                 $ translateY ((getP x-30)) 
--                 $ translateX (getT (t.+^(d^/2))) 
--                 $ scaleX (getD d) $ noteShape n
-- 
--         noteShape 1 = id
--                 $ lcA transparent
--                 $ fcA (black `withOpacity` 0.2) 
--                 $ square 1
--         noteShape 2 = lcA transparent $ fcA (green `withOpacity` 0.3) $ square 1
-- 
--         totalDur = getD $ dr{-duration ns-}
--         getT = fromRational . toRational
--         getD = fromRational . toRational
--         getP = (subtract 60) . fromIntegral . getPitch  
--                                                         
