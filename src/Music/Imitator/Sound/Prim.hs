
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

module Music.Imitator.Sound.Prim (
        UGen(..),
        Warp(..),
        Rate(..),
        Loop(..),
        DoneAction(..),
        EnvCurve(..),                 
        Envelope_Curve(..),
        Envelope(..),
  ) where

import Sound.SC3.UGen ( UGen(..), Rate(..), 
                        Warp(..), Loop(..), DoneAction(..),
                        mce, mceChannels,
                        EnvCurve(..), Envelope_Curve(..), Envelope(..),
                      )
