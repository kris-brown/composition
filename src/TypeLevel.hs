{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-
Working through https://kseo.github.io/posts/2017-01-16-type-level-functions-using-closed-type-families.html

-}
module TypeLevel where

import           Data.Proxy
import           Data.Void    (Void)
import           GHC.TypeLits

type family If c t e where
  If 'True  t e = t
  If 'False t e = e


-- type family Length xs where <--- this is not polymorphic
--  thus we need PolyKinds and to write it like below
type family Length (xs :: [k]) where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs


type family Head (xs :: [k]) where
  Head (x ': xs) = x
  Head '[] = Head '[]
type family Tail (xs :: [k]) where
 Tail (x ': xs) = xs
 Tail '[] = '[]

type family Map (f :: * -> *) (xs :: [*]) where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

-- | Type level function
type family MakePair (x :: *) where
  MakePair x = (x, x)

val :: Length '[]
val = 0
