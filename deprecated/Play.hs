-- | A simplified MID file player, as an example application using System.MIDI.
-- You will need a GM (General MIDI) capable synth, or something like that (Windows has one built-in).
--

module Play where

--------------------------------------------------------------------------------

import           Control.Concurrent
import           Control.Monad
import           Data.List
import           Data.Ord
import           System.Environment
import           System.Exit
import           System.IO

import           SMF
import           System.MIDI
import           System.MIDI.Utility

--------------------------------------------------------------------------------

-- player thread

player :: Connection -> MVar [MidiEvent] -> IO ()
player conn mv = do
  t <- currentTime conn
  evs <- readMVar mv
  case evs of
    [] -> do
      putStrLn "the song ended."
      return ()
    (MidiEvent s ev : evs') -> do
      when (s<=t) $ do
        swapMVar mv evs'
        case ev of
          SysEx _   -> return ()
          Undefined -> return ()
          _         -> send conn ev
      threadDelay 1000
      player conn mv

-- song

data Song = Song
  { songBpm    :: Float
  , songTracks :: [[MidiEvent]]
  }

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs = map (fromJust . f) $ filter test xs where
  test x = case f x of
    Just _  -> True
    Nothing -> False
  fromJust (Just x) = x

tmeta (MidiEvent' ts (Left  x)) = Just (ts,x)
tmeta (MidiEvent' _  (Right _)) = Nothing

tmidi (MidiEvent' _  (Left  _)) = Nothing
tmidi (MidiEvent' ts (Right y)) = Just $ MidiEvent (fromIntegral ts) y

toSong :: TimeBase -> [[MidiEvent']] -> Song
toSong division tracks = Song bpm $ map convert midi where
  convert = map (\(MidiEvent ts ev) -> MidiEvent (round $ pertick * fromIntegral ts) ev)
  tTempo (_,Tempo tempo) = Just tempo
  tTempo _               = Nothing
  tempos = filterMap tTempo metaAll
  (tempo,bpm) = case tempos of
    []  -> ( 500000 , 120 )
    t:_ -> ( t      , 60000000 / fromIntegral t )
  pertick = timestampUnitInMilisecs division tempo
  metaAll = concat meta
  meta = map (filterMap tmeta) tracks
  midi = map (filterMap tmidi) tracks

--------------------------------------------------------------------------------
play :: String -> IO ()
play fname = do

  ((_,division),tracks) <- loadSMF fname
  let song = toSong division tracks
  putStrLn $ "bpm = " ++ show (songBpm song)

  let events = sortBy (comparing $ \(MidiEvent t _) -> t) $ concat (songTracks song)
  mv <- newMVar events

  dst <- selectOutputDevice "please select an output device" Nothing

  conn <- openDestination dst
  start conn

  thread <- forkIO (player conn mv)

  putStrLn "Press 'ENTER' to exit."
  getLine

  killThread thread

  close conn


play' :: String -> IO ()
play' fname = do

  ((_,division),tracks) <- loadSMF fname
  let song = toSong division tracks
  putStrLn $ "bpm = " ++ show (songBpm song)

  let events = sortBy (comparing $ \(MidiEvent t _) -> t) $ concat (songTracks song)
  mv <- newMVar events

  dst <- selectOutputDevice "please select an output device" Nothing

  conn <- openDestination dst
  start conn

  thread <- forkIO (player conn mv)

  putStrLn "Press 'ENTER' to exit."
  getLine

  killThread thread

  close conn
