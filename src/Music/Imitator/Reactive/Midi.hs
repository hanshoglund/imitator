
module Music.Imitator.Reactive.Midi -- (
--   ) 
where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Newtype
import Control.Concurrent (forkIO, threadDelay)
import System.IO.Unsafe

import Music.Imitator.Reactive
import Music.Imitator.Util

import System.MIDI (MidiMessage,  MidiMessage')
import qualified System.MIDI            as Midi

type MidiName        = String
type MidiSource      = Midi.Source
type MidiDestination = Midi.Destination


midiSources :: Reactive [(MidiName, MidiSource)]
midiSources = fmap (\s -> (unsafePerformIO $ Midi.getName s,s)) 
    <$> eventToReactive 
        (pollE $ threadDelay 1 >> Midi.enumerateSources >>= return . Just)

midiDestinations :: Reactive [(MidiName, MidiDestination)]
midiDestinations = fmap (\s -> (unsafePerformIO $ Midi.getName s,s)) 
    <$> eventToReactive 
        (pollE $ threadDelay 1 >> Midi.enumerateDestinations >>= return . Just)

findSource :: Reactive String -> Reactive (Maybe MidiSource)
findSource name = g <$> name <*> midiSources
    where
        g = (\n -> listToMaybe . fmap snd . filter (\(m,_) -> isSubstringOfNormalized n m))

findDestination :: Reactive String -> Reactive (Maybe MidiDestination)
findDestination name = g <$> name <*> midiDestinations
    where
        g = (\n -> listToMaybe . fmap snd . filter (\(m,_) -> isSubstringOfNormalized n m))

midiIn :: MidiSource -> Event MidiMessage
midiIn = undefined

midiOut :: MidiDestination -> Event MidiMessage -> Event MidiMessage
midiOut = undefined




eventToReactive :: Event a -> Reactive a
eventToReactive = stepper (error "eventToReactive: ")

