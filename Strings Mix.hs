import Data.Char;import Data.List
import Data.Map (fromListWith, findWithDefault)

mix a b = intercalate"/" $ sortBy t $ filter (not.null) $ map m ['a'..'z']
  where
    f s = fromListWith (+) [(c, 1)|c <- s, isLower c]; m1 = f a; m2 = f b
    m c = let i = findWithDefault 0 c m1; j = findWithDefault 0 c m2; n=max i j
        in if n > 1 then (if i > j then "1:" else if j > i then "2:" else "=:") ++replicate n c else ""
    t x y = compare (negate(length x), take 1 x,drop 2 x) (negate (length y), take 1 y, drop 2 y)