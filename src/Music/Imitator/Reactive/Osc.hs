
module Music.Imitator.Reactive.Osc -- (
--   ) 
where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Newtype

import Music.Imitator.Reactive

import qualified Sound.OpenSoundControl as OSC

type OscMessage = OSC.Message

oscIn :: Int -> Event OscMessage
oscIn = undefined

oscOut :: String -> Int -> Event OscMessage
oscOut = undefined
