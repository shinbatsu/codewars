{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

import YonedaLemmaPreloaded
import Data.Functor.Contravariant
import Data.Void

type Hom a = (->) a

type Nat f g = forall x. f x -> g x

to :: Functor f => Nat (Hom a) f -> f a
to x = x id

from :: Functor f => f a -> Nat (Hom a) f
from x = \f -> fmap f x

type CoHom a = Op a

to' :: Contravariant f => Nat (Op a) f -> f a
to' x = x (Op id)

from' :: Contravariant f => f a -> Nat (Op a) f
from' = \x f -> contramap (getOp f) x

count1 :: forall f c x. (Functor f, Factor f, Countable c) => Count ((c -> x) -> f x)
count1 = coerce x
  where x = count :: Count (f c)
  
count2 :: forall f c x. (Contravariant f, Factor f, Countable c) => Count ((x -> c) -> f x)
count2 = coerce x
  where x = count :: Count (f c)

data Numbers = One | Two | Three deriving (Show, Eq)

instance Countable Numbers where
  count = Count 3

a :: Count ((Numbers -> Int) -> A Int) -> Count ((Numbers -> Int) -> Maybe Int)
a (Count n) = Count n

newtype A a = A { f :: Maybe a }
  deriving (Functor)
  
instance Factor A where
  factor :: forall c. (Countable c) => Count (A c)
  factor = coerce (Count (1 + n))
    where n = getCount (count @c)

b :: Count ((Int -> Numbers) -> B Int) -> Count ((Maybe Numbers -> Int) -> Int)
b (Count n) = Count n

newtype B a = B { x :: a -> Maybe Numbers }

instance Factor B where
  factor :: forall a. Countable a => Count (B a)
  factor = coerce (Count (1 + getCount (count @a)))

instance Contravariant B where
  contramap f (B g) = B (g . f)

challenge1 = a (count1 @A @Numbers)
challenge2 = b (count2 @B @Numbers)
challenge3 = Count (3^2)
challenge4 = Count 1
challenge5 = Count (3^8)