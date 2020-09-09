{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

import Kata.AdditionCommutes.Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:))

-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes a b = case (a, b) of
  (NumZ, NumZ) -> EqlZ
  (x, y) | isZero x y -> zeroSucc x y
  (NumS x, y) -> transitive (EqlS (plusCommutes x y)) (revert y x)
  
  where
    zeroSucc :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
    zeroSucc a b = case (a, b) of
      (NumZ, NumS y) -> EqlS (plusCommutes NumZ y)
      (NumS x, NumZ) -> EqlS (plusCommutes x NumZ)

    isZero :: Natural a -> Natural b -> Bool
    isZero a b = case (a, b) of
      (NumZ, NumS x) -> True; (NumS x, NumZ) -> True
      x -> False
    
    -- | For any n, n = n.
    reflexive :: Natural n -> Equal n n
    reflexive a = case a of
      NumZ -> EqlZ
      NumS m -> EqlS (reflexive m)

    -- | if a = b, then b = a.
    symmetric :: Equal a b -> Equal b a
    symmetric a = case a of
      EqlZ -> EqlZ
      EqlS x -> EqlS (symmetric x)

    transitive :: Equal a b -> Equal b c -> Equal a c
    transitive a b = case (a, b) of
      (EqlZ, EqlZ) -> EqlZ
      (EqlS x, EqlS y) -> EqlS (transitive x y)
      
    revert :: Natural m -> Natural n -> Equal (S (m :+: n)) (m :+: S n)
    revert m v = case m of
      NumZ -> EqlS (reflexive v)
      NumS x -> EqlS (revert x v)