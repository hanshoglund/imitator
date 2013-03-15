
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : GPL
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : stable
-- Portability : portable
--
-- Utility functions.
--
-------------------------------------------------------------------------------------    

module Music.Imitator.Util -- (
        -- tau,
        -- 
        -- concatWith,
        -- execute
  --) 
  where

import Data.Monoid

import System.Posix

import qualified Data.Char as Char
import qualified Data.List as List

-------------------------------------------------------------------------------------
-- String and Char
-------------------------------------------------------------------------------------

-- |
-- Synonym for 'Char.toUpper'
toUpperChar :: Char -> Char
toUpperChar = Char.toUpper

-- |
-- Synonym for 'Char.toLower'
toLowerChar :: Char -> Char
toLowerChar = Char.toLower

-- |
-- Synonym for 'fmap Char.toUpper'
toUpperString :: String -> String
toUpperString = fmap Char.toUpper

-- |
-- Synonym for 'fmap Char.toLower'
toLowerString :: String -> String
toLowerString = fmap Char.toLower

-- |
-- Convert a string to use upper case for the leading letter and lower case for
-- remaining letters.
toCapitalString :: String -> String
toCapitalString [] = []
toCapitalString (x:xs) = toUpperChar x : toLowerString xs



-------------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------------

-- |
-- Synonym for '(++)'
--
withPrefix :: [a] -> [a] -> [a]
withPrefix x = (x ++)

-- |
-- Synonym for 'flip (++)'
--
withSuffix :: [a] -> [a] -> [a]
withSuffix x = (++ x)

-- |
-- Separate a list by the given element.
--
-- Equivalent to 'List.intersperse'
--
sep :: a -> [a] -> [a]
sep = List.intersperse

-- |
-- Initiate and separate a list by the given element.
--
pre :: a -> [a] -> [a]
pre x = (x :) . sep x

-- |
-- Separate and terminate a list by the given element.
--
post :: a -> [a] -> [a]
post x = withSuffix [x] . sep x

-- |
-- Separate and terminate a list by the given element.
--
wrap :: a -> a -> [a] -> [a]
wrap x y = (x :) . withSuffix [y] . sep x

-- |
-- Combination of 'concat' and 'sep'.
--
concatSep :: [a] -> [[a]] -> [a]
concatSep x = concat . sep x

-- |
-- Combination of 'concat' and 'pre'.
--
concatPre :: [a] -> [[a]] -> [a]
concatPre x = concat . pre x

-- |
-- Combination of 'concat' and 'post'.
--
concatPost :: [a] -> [[a]] -> [a]
concatPost x = concat . post x

-- |
-- Combination of 'concat' and 'wrap'.
--
concatWrap :: [a] -> [a] -> [[a]] -> [a]
concatWrap x y = concat . wrap x y

-- |
-- Divide a list into parts of maximum length n.
--
divideList :: Int -> [a] -> [[a]]
divideList n xs
    | length xs <= n = [xs]
    | otherwise = [take n xs] ++ (divideList n $ drop n xs)

-- |
-- Break up a list into parts of maximum length n, inserting the given list as separator.
-- Useful for breaking up strings, as in @breakList 80 "\n" str@.
--
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = mconcat . List.intersperse z . divideList n

-- |
-- Break up a list into parts of maximum length n, inserting the given list as separator.
-- Useful for breaking up strings, as in @breakList 80 "\n" str@.
--
concatMapM :: (Monad f, Functor f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . mapM f

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f as = map (uncurry f) (zip is as)
    where
        n  = length as - 1
        is = [0..n]

tau :: Floating a => a
tau = 2 * pi

-- |
-- Excecute an external process asynchronously (using @forkProcess@) with the given arguments.
--
execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()


