import Data.Array

explosiveSum :: Integer -> Integer
explosiveSum n = p ! (n,n)
  where
    p = array ((0,0),(n,n)) [((i,j), v i j) | i <- [0..n], j <- [0..n]]
    v 0 _ = 1
    v i 0 = 0
    v i j | j > i     = p ! (i,i)
          | otherwise = p ! (i,j-1) + p ! (i-j,j)