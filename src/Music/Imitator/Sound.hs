
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

module Music.Imitator.Sound where

import Data.IORef
import Data.Monoid
import Data.List (intersperse)
import Control.Applicative
import Control.Concurrent (threadDelay)

import Sound.SC3.Server
import Sound.SC3.Server.FD
import Sound.SC3.UGen
import Sound.OSC.Transport.FD.UDP (openUDP)
import Sound.OpenSoundControl.Type
import Sound.OSC.Transport.FD.UDP (UDP)

import Music.Imitator.Util

sineG :: UGen -> UGen
sineG fq  = sinOsc AR fq 0 * 0.05

impG :: UGen -> UGen
impG fq = impulse AR fq 0

mouseG :: (UGen, UGen)
mouseG    = (mouseX KR 0 1 Linear 0, mouseY KR 0 1 Linear 0)

decodeG :: UGen -> UGen
decodeG input = decodeB2 kNumSpeakers w x y 0
    where
        [w,x,y,_] = mceChannels input


foaPanB :: UGen -> UGen -> UGen -> UGen
foaPanB azimuth elev input = mkFilter "FoaPanB" [input, azimuth, elev] 4

foaRotate :: UGen -> UGen -> UGen
foaRotate angle input = mkFilter "FoaRotate" [w,x,y,z,angle] 4
    where
        [w,x,y,z] = mceChannels input


impulseTest :: UGen
impulseTest = decodeG $ foaRotate ((fst mouseG + 1) * tau + (tau/8)) $ foaPanB 0 0 $ (impG 12 * 0.5)

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













test :: UGen -> IO ()
test gen = do
    sendStd $ d_recv synthDef

    threadDelay $ 1000000 `div` 2
    -- TODO proper async wait here

    sendStd $ addToTailMsg
        where
            addToTailMsg    = s_new "testGen" 111 AddToTail 0 []
            synthDef        = synthdef "testGen" (out 0 $ gen * kMaster)

startServer :: IO ()
startServer = execute "scsynth" ["-v", "1", "-u", show kStdPort]

stopServer :: IO ()
stopServer = execute "killall" ["scsynth"]

printServerStatus :: IO ()
printServerStatus = withSC3 serverStatus <$$> (mconcat . intersperse "\n" . (++["\n"])) >>= putStr
    where
        x <$$> f = f <$> x

abort :: IO ()
abort = sendStd $ g_deepFree [0]

sendStd :: Message -> IO ()
sendStd msg = do
    t <- kStdTransport
    send t msg











-- Note: withSC3 using kStdPort by default

kStdTransport :: IO UDP
kStdTransport = openUDP "127.0.0.1" kStdPort

kStdPort :: Int
kStdPort = 57110

kMaster :: UGen
kMaster = 0.3

kNumSpeakers :: Int
kNumSpeakers = 8

