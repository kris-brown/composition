{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTSyntax     #-}
{-# LANGUAGE LambdaCase     #-}


module Primitive (
  Frequency,
  Note,
  PitchClass,
  NoteType,
  Primitive

) where

import           Control.DeepSeq        (NFData)
import           Enumerate              (Enumerable, enumerated)
import           Enumerate.Cardinality  (Finite)
import           Euterpea               (Music, ToMusic1, a, af, as, b, bf, bn,
                                         bs, c, cf, cs, d, df, ds, e, ef, en,
                                         es, f, ff, fs, g, gf, gs, hn, play, qn,
                                         sn, wn)
import qualified Euterpea               as ME (Dur, Octave, Pitch)
import           Generic.Random.Generic (genericArbitrary, uniform)
import           GHC.Generics           (Generic)
import           Test.QuickCheck        (Arbitrary (..), CoArbitrary (..),
                                         genericCoarbitrary)
import           Utilities              (Playable (mkMusic), toEnum')

  -- mkMusic :: forall b. (NFData b,ToMusic1 b) => a -> Music b

----------------------
-- Datatypes
----------------------
type Frequency = Float -- Unit: Hz

-- | Valid letters for notes/keys in music
data Letter = A | B | C | D | E | F | G
  deriving (Eq,Enum,Ord,Show,Generic)

instance Enumerable Letter
instance Finite Letter

-- | Qualifier for letter to specify a note
data Accidental = Natural | Sharp | Flat
  deriving (Eq,Enum,Ord,Show,Generic)

instance Finite Accidental
instance Enumerable Accidental


-- | Representation of a pitch within a key (possible to have enharmonicity)
data Note = Note Letter Accidental
  deriving (Eq,Ord,Show,Generic)

instance Finite Note
instance Enumerable Note

-- | Possible pitches mod 12, with representatives chosen to prevent enharmonicity
data PitchClass = A' | Bb | B' | C' | Cs | D' | Eb | E' | F' | Fs | G' | Ab
  deriving (Eq,Enum,Ord,Show,Generic)

instance Finite PitchClass
instance Enumerable PitchClass

instance Arbitrary PitchClass where
  arbitrary = genericArbitrary uniform

instance Num PitchClass where
  fromInteger = toEnum' 12 . fromIntegral
  a + b = toEnum' 12 $ fromEnum a + fromEnum b
  a - b = toEnum' 12 $ fromEnum a - fromEnum b
  a * b = toEnum' 12 $ fromEnum a * fromEnum b
  abs    = id
  signum = id

instance Playable PitchClass where
  mkMusic pc  = play $ mkNote pc 4 qn

fromFreq :: Frequency -> PitchClass
fromFreq f = toEnum $ round $ 12 * logBase 2 (f/440)

-- | E.g. C4, A5, Eb6
data Pitch = Pitch PitchClass Int
  deriving (Eq,Ord,Show,Generic)


instance Playable Pitch where
  mkMusic (Pitch pc i) = play $ mkNote pc i qn

instance CoArbitrary PitchClass where
  coarbitrary = genericCoarbitrary

a440 = Pitch A' 4




-- | Simplification of the types of notes we can have (ignoring ornaments, etc.)
data NoteType =  Sixteenth | Eighth | Quarter | Half | Whole

instance Playable NoteType where
  mkMusic nt = play $ c 4 (mkDur nt)


-- | The simplest type of thing that can be rendered into a sound
--   It has pitch and duration
data Primitive = Primitive Pitch NoteType



mkNote :: PitchClass -> ME.Octave -> ME.Dur -> Music ME.Pitch
mkNote = \case
 A' -> a
 Bb -> bf
 B' -> b
 C' -> c
 Cs -> cs
 D' -> d
 Eb -> ef
 E' -> e
 F' -> f
 Fs -> fs
 G' -> g
 Ab -> af

mkDur :: NoteType -> ME.Dur
mkDur = \case
  Sixteenth -> sn
  Eighth -> en
  Quarter -> qn
  Half -> hn
  Whole -> wn

instance Playable Primitive where
  mkMusic (Primitive (Pitch pc i) nt) = play $ mkNote pc i (mkDur nt)
