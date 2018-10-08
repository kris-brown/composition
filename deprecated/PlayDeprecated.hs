{-

Don't know how to make this work yet: get undefined error somewhere in hmidi
-}
module PlayDeprecated where

import           System.MIDI      (close, enumerateDestinations, getEvents,
                                   openDestination, send, start, stop)
import           System.MIDI.Base


test :: IO ()
test = do
  ds   <- enumerateDestinations
  conn <- openDestination $ head ds
  start conn
  send conn $ MidiMessage 1 (NoteOn 0 10)
  es <- getEvents conn
  print es
  stop conn
  close conn
