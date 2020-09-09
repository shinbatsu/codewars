{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' g a = case g of
  EqlZ -> reflexive a
  EqlS s -> EqlS (plus' s a)

add :: Natural m -> Natural n -> Natural (m :+: n)
add m n = case m of
  NumZ -> n
  NumS x -> NumS (add x n)

multiply :: Natural m -> Natural n -> Natural (m :*: n)
multiply m n = case m of
  NumZ -> NumZ
  NumS x -> add n (x`multiply`n)

reflexive :: Natural n -> Equal n n
reflexive a = case a of
  NumZ -> EqlZ
  NumS m -> EqlS (reflexive m)
  
symmetric :: Equal a b -> Equal b a
symmetric a = case a of
  EqlZ -> EqlZ
  EqlS x -> EqlS (symmetric x)

transitive :: Equal a b -> Equal b c -> Equal a c
transitive x y = case (x, y) of
  (EqlZ, EqlZ) -> EqlZ
  (EqlS a, EqlS b) -> EqlS (a`transitive`b)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc a b c = case a of
  NumS x -> EqlS (plusAssoc x b c)
  NumZ -> reflexive (f b c)
  where
    f :: Natural x -> Natural y -> Natural (x :+: y)
    f x y = case x of
      NumZ -> y
      NumS v -> NumS (f v y)
    
-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm:: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm a b = case (a, b) of
  (NumZ, NumZ) -> EqlZ
  (x, y) | isZero x y -> zeroSucc x y
  (NumS x, y) -> transitive (EqlS (plusComm x y)) (revert y x)
  where
    zeroSucc :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
    zeroSucc a b = case (a, b) of
      (NumZ, NumS y) -> EqlS (plusComm NumZ y)
      (NumS x, NumZ) -> EqlS (plusComm x NumZ)
      
    isZero :: Natural a -> Natural b -> Bool
    isZero a b = case (a, b) of
      (NumZ, NumS x) -> True; (NumS x, NumZ) -> True
      x -> False
      
    transitive :: Equal a b -> Equal b c -> Equal a c
    transitive a b = case (a, b) of
      (EqlZ, EqlZ) -> EqlZ
      (EqlS x, EqlS y) -> EqlS (transitive x y)
      
    revert :: Natural m -> Natural n -> Equal (S (m :+: n)) (m :+: S n)
    revert m v = case m of
      NumZ -> EqlS (reflexive v)
      NumS x -> EqlS (revert x v)
      
-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm a = case a of
  NumS x -> zeroComm x
  NumZ -> EqlZ

timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm a b = case (a, b) of
  (NumZ, g) -> zeroComm g
  (g, NumZ) -> symmetric (zeroComm g)
  (s@(NumS g), q@(NumS v)) ->
    let in EqlS (p(p(p(p(p(f v(g`timesComm`q))(f v(f g(v`timesComm`g))))(plusAssoc v g(g`multiply`v)))(plus'(plusComm v g)(g`multiply`v)))(symmetric(plusAssoc g v(g`multiply`v))))(f g(timesComm s v)))
  where
    p :: Equal a b -> Equal b c -> Equal a c
    p x y = case (x, y) of
      (EqlZ, EqlZ) -> EqlZ
      (EqlS a, EqlS b) -> EqlS (p a b)
    f :: Natural a -> Equal m n -> Equal (a :+: m) (a :+: n)
    f a e = case a of
      NumZ -> e
      NumS g -> EqlS (f g e)