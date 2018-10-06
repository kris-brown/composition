{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTSyntax    #-}

module Lib  where

import           Enumerate              (Enumerable, enumerated)
import           Enumerate.Cardinality  (Finite)
import           Generic.Random.Generic (genericArbitrary, uniform)
import           GHC.Generics           (Generic)
import           Test.QuickCheck        (Arbitrary (..), CoArbitrary (..),
                                         genericCoarbitrary)

---------------
-- Utilities
--------------
toEnum' :: Enum a => Int -> Int -> a
toEnum' m n = toEnum (n `mod` m)

-- | Special case in which we can visually represent a function a->b
showFunc :: (Enumerable a, Show a, Show b) => (a -> b) -> String
showFunc f = show (g <$> enumerated)
  where g x = show x ++ "->" ++ show (f x)
  -- | Special case in which we can determine equality for a function a->b
eqFunc :: (Enumerable a, Eq a, Eq b) => (a -> b) -> (a -> b) -> Bool
eqFunc f g = (f <$> enumerated) == (g <$> enumerated)

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

fromFreq :: Frequency -> PitchClass
fromFreq f = toEnum $ round $ 12 * logBase 2 (f/440)

-- | E.g. C4, A5, Eb6
data Pitch = Pitch PitchClass Int
  deriving (Eq,Ord,Show,Generic)

instance CoArbitrary PitchClass where
  coarbitrary = genericCoarbitrary


-------------------------------
-- | A voice leading in general, which maps each pitch class to another
data Lead where
  Lead :: (PitchClass -> PitchClass) -> Lead
  deriving (Generic)

instance Finite Lead

instance Show Lead where
  show (Lead f) = showFunc f
instance Eq Lead where
  (Lead f) == (Lead g) = eqFunc f g

instance Arbitrary Lead where
  arbitrary = genericArbitrary uniform
-------------------------------
data Exposition  = Exposition
data Development = Development
data Recap  = Recap
data Section= Section
data Var    = Var

data Piece = Sonata Exposition Development Recap | Rondo Section Section Section
            | ThemeVar Section Var Var Var
