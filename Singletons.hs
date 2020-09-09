{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)
data Nat = Zero | Succ Nat
data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool where
  m :< Zero = False
  Zero :< Succ n = True
  (Succ m) :< (Succ n) = m :< n
type family If (cond ::Bool) (t::k) (f::k):: k where
  If True  t f = t
  If False t f = f
type family IsZero (n::Nat):: Bool where
  IsZero Zero = True
  IsZero (Succ _) = False
type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min Zero b = Zero
  Min a Zero = Zero
  Min (Succ a) (Succ b) = Succ (Min a b)
type family Sub (a :: Nat) (b :: Nat) :: Nat where
  Sub Zero b = Zero
  Sub a Zero = a
  Sub (Succ a) (Succ b) = Sub a b
type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add Zero b = b
  Add (Succ a) b = Succ (Add a b)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
xs ++ as = case xs of
  VNil -> as
  VCons x xs' -> VCons x (xs' ++ as)
index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index n v = case (n, v) of
  (SZero, VCons x _) -> x
  (SSucc a, VCons _ xs) -> index a xs
map :: (a -> b) -> Vec a n -> Vec b n
map f v = case v of
  VNil -> VNil
  VCons x xs -> VCons (f x) (map f xs)
replicate :: s -> SNat a -> Vec s a
replicate s n = case n of
  SZero -> VNil
  SSucc k -> VCons s (replicate s k)
zipWith :: (x -> y -> z) -> Vec x a -> Vec y a -> Vec z a
zipWith f a b = case (a, b) of
  (VNil, VNil) -> VNil
  (VCons x xs, VCons y as) -> VCons (f x y) (zipWith f xs as)
take :: SNat a -> Vec v n -> Vec v (Min a n)
take n v = case (n, v) of
  (_, VNil) -> VNil
  (SZero, _) -> VNil
  (SSucc k, VCons x xs) -> VCons x (take k xs)
drop :: SNat n -> Vec a m -> Vec a (Sub m n)
drop n v = case (n, v) of
  (SZero, xs) -> xs
  (_, VNil) -> VNil
  (SSucc k, VCons _ xs) -> drop k xs
head :: Vec a b -> a
head v = case v of
  VCons x _ -> x
tail :: Vec a (Succ b) -> Vec a b
tail v = case v of
  VCons _ xs -> xs
