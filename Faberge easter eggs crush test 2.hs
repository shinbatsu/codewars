
import Data.Array
import Data.List (foldl')
mo = 998244353

height :: Integer -> Integer -> Integer
height n m = fst $ foldl' iter (0, 1) [1..n]
  where
    n_cache :: Array Integer Integer
    n_cache = listArray (0, 80000) $
      [0, 1] ++ [ (n_cache ! (mo `mod` i) * (mo - mo `div` i)) `mod` mo | i <- [2..80000] ]
    m' = m `mod` mo
    iter (h, t) i =
      let t' = (t * (m' - i + 1) `mod` mo * (n_cache ! fromInteger i)) `mod` mo
          h' = (h + t') `mod` mo
      in (h', t')