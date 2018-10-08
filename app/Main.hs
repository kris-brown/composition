module Main where

import           Data.Tagged           (Tagged)
import           Enumerate.Cardinality (reifyCardinality)
import           Lib                   (Lead)
import           Primitive             (PitchClass)
import           Test.QuickCheck       (Gen, arbitrary)
import           Utilities             (sample, samplePlay)

main :: IO ()
main = do
  putStrLn "Some random PitchClasses "
  samplePlay 3 (arbitrary :: Gen PitchClass)
  putStrLn $ "Cardinality of 'PitchClass' is "++show (reifyCardinality ( [] ::  [PitchClass] ))

  putStrLn "Some random voice leadings "
  is <- sample 3 (arbitrary :: Gen Lead)
  print is
  putStrLn $ "Cardinality of 'Lead' is "++show (reifyCardinality ( []::  [Lead] ))
