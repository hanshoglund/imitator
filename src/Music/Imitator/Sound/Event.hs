
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- Sound backend (implemented as a wrapper around hsc3).
--
-------------------------------------------------------------------------------------

module Music.Imitator.Sound.Event -- (
-- )
  where

-- type Source a = Event a
-- type Sink a   = Event a -> Event a
--       
--       
-- play :: Event UGen -> Event ()
-- abort :: Event () -> Event ()
-- sendStd :: Event Message -> Event ()
-- -- newBuffer :: Event Int -> Event Int -> Event Int -> Event ()
-- -- readBuffer :: Event Int -> Event String -> Event Int -> Event Int -> Event  ()
-- -- closeBuffer :: Event Int -> Event IO ()
-- runServer :: NRT -> FilePath -> FilePath -> IO ()
-- startServer :: Event () -> Event ()
-- stopServer :: Event () -> Event ()
-- serverActive :: Reactive Bool
-- serverUgens :: Reactive Int
-- serverSynths :: Reactive Int
-- serverGroups :: Reactive Int
-- serverInstruments :: Reactive Int
-- serverCPUAverage :: Reactive Double
-- serverCPUPeak :: Reactive Double
-- serverSampleRateNominal :: Reactive Double
-- serverSampleRateActual :: Reactive Double
-- printServerStatus :: Reactive ()
