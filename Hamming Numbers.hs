hamming :: Int -> Int
hamming n = h !! (n - 1)
h = ma
  where
    ma = 1 : m (map (*2) ma) (m (map (*3) ma) (map (*5) ma))
    m (x:xs) (y:ys)
      | x < y     = x : m xs (y:ys)
      | x > y     = y : m (x:xs) ys
      | otherwise = x : m xs ys