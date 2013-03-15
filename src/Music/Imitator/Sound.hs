
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

module Music.Imitator.Sound (
        
        -- TODO remove these
        impulseTest,

        -- * Generators
        sinG,
        impulseG,
        mouseG,




        -- ** Spacialization
        -- *** Encoders
        foaPanB,
        foaOmniG,

        -- *** Decoders
        decodeG,       

        -- *** Transformations
        foaRotate,
        foaTilt,
        foaTumble,

        -- ** Utilities
        numChannels,



        -- ** Control
        -- TODO



        -- ** Play and stop
        play,
        abort,
        sendStd,

        -- *** Server status
        startServer,
        stopServer,
        serverActive,
        serverStatus,
        serverStatusData,
        serverUgens,
        serverSynths,
        serverGroups,
        serverInstruments,
        serverCPUAverage,
        serverCPUPeak,
        serverSampleRateNominal,
        serverSampleRateActual,

        printServerStatus,
        
        -- ** Constants
        kStdServer,
        kStdPort,
        kMaster,
        kOutputOffset,
        kNumSpeakers,        
  ) where

import Data.Monoid        
import Control.Exception (try, SomeException)
import Control.Applicative
import Control.Concurrent (threadDelay)

import Sound.SC3.UGen
import qualified Sound.SC3.Server.FD as S

import Sound.OSC.Transport.FD.UDP (openUDP)
import Sound.OpenSoundControl.Type (Message, Datum(..))
import Sound.OSC.Transport.FD.UDP (UDP)

import Music.Imitator.Util




impulseTest :: UGen
impulseTest = decodeG kNumSpeakers $ foaRotate ((fst mouseG + 1) * tau + (tau/8)) $ foaPanB 0 0 $ (impulseG 12 * 0.5)







-- |
-- Sinusoid generator.
--
-- > sinG freq                 
--
sinG :: UGen -> UGen
sinG freq  = sinOsc AR freq 0 * 0.05

-- |
-- Impulse generator.
--
-- > sinG freq                 
--
impulseG :: UGen -> UGen
impulseG freq = impulse AR freq 0

-- |
-- Mouse position generator.
--
-- > let (x, y) = mouseG                 
--
mouseG :: (UGen, UGen)
mouseG    = (mouseX KR 0 1 Linear 0, mouseY KR 0 1 Linear 0)



foaOmniG :: UGen -> UGen
foaOmniG input = mce $ replicate 4 input

foaPanB :: UGen -> UGen -> UGen -> UGen
foaPanB azimuth elev input = mkFilter "FoaPanB" [input, azimuth, elev] 4

decodeG :: Int -> UGen -> UGen
decodeG numSpeakers input = decodeB2 numSpeakers w x y 0
    where
        [w,x,y,_] = mceChannels input


foaRotate :: UGen -> UGen -> UGen
foaRotate angle input = mkFilter "FoaRotate" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = mceChannels input

foaTilt :: UGen -> UGen -> UGen
foaTilt angle input = mkFilter "FoaTilt" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = mceChannels input

foaTumble :: UGen -> UGen -> UGen
foaTumble angle input = mkFilter "FoaTumble" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = mceChannels input


numChannels :: UGen -> Int
numChannels = length . mceChannels






{-
,U "FoaAsymmetry" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) asymmetry transformer"
,U "FoaDirectO" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDirectZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) directivity transformer"
,U "FoaDominateX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaDominateY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaDominateZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "gain" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) dominance transformer"
,U "FoaFocusX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaFocusY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaFocusZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) focus transformer"
,U "FoaNFC" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "distance" (1) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) nearfield compensation filter"
,U "FoaPanB" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "azimuth" (0) Nothing,I (2,2) "elevation" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) panner"
,U "FoaPressX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaPressY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaPressZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) press transformer"
,U "FoaProximity" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "distance" (1) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) proximity effect filter"
,U "FoaPsychoShelf" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "freq" (400) Nothing,I (2,2) "k0" (0) Nothing,I (3,3) "k1" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) psychoacoustic shelf filter"
,U "FoaPushX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaPushY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaPushZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) push transformer"
,U "FoaRotate" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaTilt" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaTumble" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) rotation transformer"
,U "FoaZoomX" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
,U "FoaZoomY" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
,U "FoaZoomZ" [ AR ] AR Nothing [ I (0,0) "in" (0) Nothing,I (1,1) "angle" (0) Nothing ] Nothing (Left 4) "First Order Ambisonic (FOA) zoom transformer"
-}













-- |
-- Add a generator to the synthesis graph.
--
play :: UGen -> IO ()
play gen = do
    active <- serverActive
    case (active) of
        False -> fail "Server not running"
        True  -> return ()

    sendStd $ S.d_recv synthDef
    threadDelay $ second `div` 2   -- TODO proper async wait here
    sendStd $ addToTailMsg

        where           
            second          = 1000000
            addToTailMsg    = S.s_new "playGen" 111 S.AddToTail 0 []
            synthDef        = S.synthdef "playGen" (out kOutputOffset $ gen * kMaster)

-- |
-- Abort all current synthesis (remove generators from the synthesis graph).
--
abort :: IO ()
abort = sendStd $ S.g_deepFree [0]

-- |
-- Send a message to the server over the standard connection.
--
sendStd :: Message -> IO ()
sendStd msg = do
    fd <- kStdServer
    S.send fd msg

-- |
-- Start the server.
--
startServer :: IO ()
startServer = execute "scsynth" ["-v", "1", "-u", show kStdPort]

-- |
-- Stop the server.
--
stopServer :: IO ()
stopServer = execute "killall" ["scsynth"]


-- |
-- Return whether the server is active.
--
serverActive :: IO Bool
serverActive = do
    status <- try serverStatus :: IO (Either SomeException [String])
    case status of
        Left _  -> return False
        Right _ -> return True

-- |
-- Get server status.
--
serverStatus :: IO [String]
serverStatus = S.withSC3 S.serverStatus

-- |
-- Get server status as a raw message.
--
serverStatusData :: IO [Datum]
serverStatusData = S.withSC3 S.serverStatusData

serverUgens                 :: IO Int
serverSynths                :: IO Int
serverGroups                :: IO Int
serverInstruments           :: IO Int
serverCPUAverage            :: IO Double
serverCPUPeak               :: IO Double
serverSampleRateNominal     :: IO Double
serverSampleRateActual      :: IO Double
serverUgens                 = serverStatusData >>= return . getDatumInt . (!! 1)
serverSynths                = serverStatusData >>= return . getDatumInt . (!! 2)
serverGroups                = serverStatusData >>= return . getDatumInt . (!! 3)
serverInstruments           = serverStatusData >>= return . getDatumInt . (!! 4)
serverCPUAverage            = serverStatusData >>= return . getDatumFloat . (!! 5)
serverCPUPeak               = serverStatusData >>= return . getDatumFloat . (!! 6)
serverSampleRateNominal     = serverStatusData >>= return . getDatumDouble . (!! 7)
serverSampleRateActual      = serverStatusData >>= return . getDatumDouble . (!! 8)

getDatumInt (Int x) = x
getDatumFloat (Float x) = x
getDatumDouble (Double x) = x



-- |
-- Print information about the server.
--
printServerStatus :: IO ()
printServerStatus = do
    status <- serverStatus
    msg <- return $ concatLines status
    putStrLn msg












-- |
-- Standard server connection.
--
-- This is the same as 'withSC3' uses.
-- 
kStdServer :: IO UDP
kStdServer = openUDP "127.0.0.1" kStdPort

-- |
-- Standard server port.
-- 
kStdPort :: Int
kStdPort = 57110

-- |
-- Global master volume (low while developing).
-- 
kMaster :: UGen
kMaster = 0.3

-- |
-- Global speaker offset (for skipping hardware channels etc).
-- 
kOutputOffset :: UGen
kOutputOffset = 0

-- |
-- Number of speakers used by decoders.
-- 
kNumSpeakers :: Int
kNumSpeakers = 8

