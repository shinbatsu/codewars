{-# LANGUAGE GADTs, DataKinds, TypeFamilies, UndecidableInstances #-}

module OddsAndEvens where
-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
  -- | Zero is even.
  ZeroEven :: Even Z
  -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
  -- | One is odd.
  OneOdd :: Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven $ oddPlusOne n

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family A (n :: Nat) (m :: Nat) :: Nat where {
  A Z m = m; A (S n) m = S (A n m)
}

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (A n m)
evenPlusEven n m = case n of
  ZeroEven -> m
  NextEven z -> NextEven $ evenPlusEven z m

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (A n m)
oddPlusOdd n m = case n of
  OneOdd -> oddPlusOne m
  NextOdd z -> NextEven $ oddPlusOdd z m

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (A n m)
evenPlusOdd n m = case n of
  ZeroEven -> m
  NextEven z -> NextOdd $ evenPlusOdd z m

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (A n m)
oddPlusEven n m = case n of
  OneOdd -> evenPlusOne m
  NextOdd z -> NextOdd $ oddPlusEven z m

-- | Multiplies two natural numbers.
type family M (n :: Nat) (m :: Nat) :: Nat where {
  M Z m= Z; M (S n) m = A (M n m) m
}
-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (M n m)
evenTimesEven n m = case n of
  ZeroEven -> ZeroEven
  NextEven z ->
    let p = evenTimesEven z m
        a = evenPlusEven p m
        b = evenPlusEven a m
    in b

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (M n m)
oddTimesOdd n m = case n of
  OneOdd -> m
  NextOdd z ->
    let p = oddTimesOdd z m
        t = oddPlusOdd p m
        r = evenPlusOdd t m
    in r

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (M n m)
evenTimesOdd n m = case n of
  ZeroEven -> ZeroEven
  NextEven z ->
    let p = evenPlusOne z
        t = oddTimesOdd p m
        r = oddPlusOdd t m
    in r

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (M n m)
oddTimesEven n m = case n of
  OneOdd -> m
  NextOdd z ->
    let p = oddPlusOne z
        t = evenTimesEven p m
        r = evenPlusEven t m
    in r