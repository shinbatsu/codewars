{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}

import Kata.AdditionAssoc.Definitions

reflexive :: Natural n -> Equal n n
reflexive n = case n of
  NumZ -> EqlZ
  NumS m -> EqlS (reflexive m)
symmetric :: Equal a b -> Equal b a
symmetric eq = case eq of
  EqlZ -> EqlZ
  EqlS p -> EqlS (symmetric p)
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc (NumS a) b c = EqlS (plusAssoc a b c)
plusAssoc NumZ b c = reflexive (f b c)
  where
    f :: Natural x -> Natural y -> Natural (x :+: y)
    f NumZ y = y; f (NumS x) y = NumS (f x y)