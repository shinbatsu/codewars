import Data.List (permutations, sort, nub)

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse (step n)
  where
    step 0 = []
    step x = fromIntegral (x `mod` 10) : step (x `div` 10)

isDescending :: Int -> Bool
isDescending n
  | n <= 10 = True
  | otherwise = all (uncurry (>=)) $ zip ds (tail ds)
  where
    ds = digits n

nextBigger :: Int -> Int
nextBigger n
  | n <= 11 || isDescending n = -1
  | otherwise = findNext (show n) 2
  where
    findNext s i
      | i > length s = -1
      | otherwise =
          let (h, t) = splitAt (length s - i) s
              perms = sort . nub $ map (h ++) (permutations t)
              result = dropWhile (<= s) perms
          in if null result
               then findNext s (i + 1)
               else read (head result)