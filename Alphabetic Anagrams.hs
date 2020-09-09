import Data.List

factorial 0 = 1
factorial n = n * factorial (n - 1)

lexiPos :: String -> Integer
lexiPos word = lexiPos' word 0
  where
    lexiPos' :: String -> Integer -> Integer
    lexiPos' [] ct = ct + 1
    lexiPos' (x:xs) ct = 
      let pos = factorial (fromIntegral (length (x:xs)))
          count x' = fromIntegral (length (filter (== x') (x:xs))) 
          pos' = pos `div` (product [factorial (fromIntegral (count c)) | c <- unique (x:xs)])
          ct' = ct + sum [pos' `div` (fromIntegral (length (x:xs))) * count c | c <- unique (x:xs), c < x]
      in lexiPos' xs ct'
    unique = foldr (\x acc -> if x `elem` acc then acc else x:acc) []