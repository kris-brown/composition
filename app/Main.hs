module Main where

import           Data.Tagged           (Tagged)
import           Enumerate.Cardinality (reifyCardinality)
import           Lib
import           Test.QuickCheck       (Gen, arbitrary, sample')

main :: IO ()
main = do
  ns <- sample' (arbitrary :: Gen PitchClass)
  putStrLn $ "Some random PitchClasses "++ show ns
  putStrLn $ "Cardinality of 'PitchClass' is "++show (reifyCardinality ( [] ::  [PitchClass] ))
  is <-  sample' (arbitrary :: Gen Lead)
  putStrLn $ "Some random voice leadings "++ show is
  putStrLn $ "Cardinality of 'Lead' is "++show (reifyCardinality ( []::  [Lead] ))
