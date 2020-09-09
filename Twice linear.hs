import Data.Set as Set
dblLinear n = g Set.empty 1 n
  where
    g set u n
      | n == 0    = u
      | otherwise = g ns nextU (n - 1)
      where
        ns = Set.insert (2 * u + 1) . Set.insert (3 * u + 1) $ Set.deleteMin set
        nextU = Set.findMin ns