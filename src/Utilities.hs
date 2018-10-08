
module Utilities where
import           Control.Concurrent (threadDelay)
import           Control.Monad      (replicateM)
import           Enumerate          (Enumerable, enumerated)
import           Test.QuickCheck    (Arbitrary, Gen, arbitrary, generate)
---------------
-- Utilities
--------------

class Playable a where
  mkMusic :: a -> IO ()

--------------
sample :: Int -> Gen a -> IO [a]
sample i = sequence . replicateM i generate

samplePlay :: (Playable a, Show a) => Int -> Gen a -> IO ()
samplePlay i a = do xs <- sample i a
                    mapM_ f xs
 where f x = do print x
                mkMusic x
                threadDelay 3
--------------

toEnum' :: Enum a => Int -> Int -> a
toEnum' m n = toEnum (n `mod` m)
--------------

-- | Special case in which we can visually represent a function a->b
showFunc :: (Enumerable a, Show a, Show b) => (a -> b) -> String
showFunc f = show (g <$> enumerated)
  where g x = show x ++ "->" ++ show (f x)
  -- | Special case in which we can determine equality for a function a->b
eqFunc :: (Enumerable a, Eq a, Eq b) => (a -> b) -> (a -> b) -> Bool
eqFunc f g = (f <$> enumerated) == (g <$> enumerated)

--------------
