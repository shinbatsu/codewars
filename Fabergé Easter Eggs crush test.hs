heigth :: Integer -> Integer -> Integer
heigth n m = pascal 1 0 1 m
  where
    pascal :: Integer -> Integer -> Integer -> Integer -> Integer
    pascal i res k m
      | i > n     = res
      | otherwise =
          let k' = quot (k * m) i
          in pascal (i + 1) (res + k') k' (m - 1)