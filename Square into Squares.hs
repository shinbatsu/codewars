attempt a b =
  if b < 1 then Nothing else if b * b == a then Just [b]
  else maybe (attempt a (b - 1)) (Just . (b :)) (attempt (a - b * b) (min (b - 1) (sqr (a - b * b))))
sqr c = f 0 c
  where
    f l h
      | l > h = h
      | m * m > c = f l (m - 1)
      | otherwise = f (m + 1) h
      where m = div (l + h) 2
decompose :: Integer -> Maybe [Integer]
decompose n = case attempt (n^2) (n - 1) of
  Nothing -> Nothing
  Just a -> Just (reverse a)