properFractions :: Integer -> Integer
properFractions d
  | d <= 1    = 0
  | otherwise = phi d
  where
    phi n = foldr (\p acc -> acc - (div acc p)) n (primeFactors n)
    primeFactors n = factor n 2
      where
        factor d p
          | p * p > d      = if d > 1 then [d] else []
          | mod d p == 0 = p : factor (removeAll d p) (next p)
          | otherwise      = factor d (next p)
        removeAll d p
          | mod d p == 0 = removeAll (div d p) p
          | otherwise      = d
        next 2 = 3
        next p = p + 2