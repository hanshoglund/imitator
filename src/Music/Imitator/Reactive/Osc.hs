
module Music.Imitator.Reactive.Osc (
        module Sound.OpenSoundControl,

        -- * Basic types
        OscTime,
        OscPacket,
        OscMessage,
        OscBundle,

        -- * Sending and receiving
        -- ** UDP
        oscInUdp,
        oscOutUdp,
  ) where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Newtype
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkIO, forkOS, threadDelay)

import Music.Imitator.Reactive
import Music.Imitator.Util

import Sound.OpenSoundControl hiding (Time, time)
import qualified Sound.OSC.FD as Osc

type OscTime    = Osc.Time

type OscPacket  = Osc.Packet
type OscMessage = Osc.Message
type OscBundle  = Osc.Bundle

-- |
-- Recieve OSC from the given port.
--
-- > oscInUdp port
--
oscInUdp :: Int -> Event OscPacket
oscInUdp port = unsafePerformIO $ do
    (k, e) <- newSource
    fd <- Osc.udpServer "127.0.0.1" port
    forkIO $ do
        Osc.recvPacket fd >>= k
    return e

-- |
-- Send OSC to the given address.
--
-- > oscInUdp address port
--
oscOutUdp :: OSC a => String -> Int -> Event a -> Event a
oscOutUdp addr port = putE $ \msg -> do
    Osc.sendOSC dest msg
        where
            dest = unsafePerformIO $ do
                -- putStrLn "Osc.openUDP"
                Osc.openUDP addr port

{-
oscIn  :: Osc.Transport t => t -> Event OscPacket
oscIn = undefined

oscOut :: (OSC a, Osc.Transport t) => t -> Event a -> Event a
oscOut dest =         -}


