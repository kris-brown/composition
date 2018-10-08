{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTSyntax    #-}

module Lib (Lead) where

import           Enumerate              (Enumerable, enumerated)
import           Enumerate.Cardinality  (Finite)
import           Generic.Random.Generic (genericArbitrary, uniform)
import           GHC.Generics           (Generic)
import           Test.QuickCheck        (Arbitrary (..), CoArbitrary (..),
                                         genericCoarbitrary)

import           Primitive              (PitchClass)
import           Utilities              (eqFunc, showFunc)
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
