
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

module Music.Imitator.Util (
        tau,


        execute
  ) where


import System.Posix


tau :: Floating a => a
tau = 2 * pi



execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()


