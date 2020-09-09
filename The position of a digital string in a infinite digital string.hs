findPosition :: String -> Integer
findPosition s
  | all (== '0') s = pos (read ("1" ++ s) - 1) + 1
  | otherwise      = let (pa, _, _) = loop 1 lenStr naInit 0 False in pa
  where
    lenStr = toInteger $ length s
    naInit = 10 ^ lenStr
    pos n =
      let sn = show n
          lenSn = toInteger $ length sn
          off = sum [9 * 10^(i - 1) * i | i <- [1 .. lenSn - 1]]
          base = read ("0" ++ replicate (fromInteger (lenSn - 1)) '9') :: Integer
      in off + (n - base) * lenSn
    findSubstr needle haystack = go 0 haystack
      where
        lenN = length needle
        go i xs
          | length xs < lenN = -1
          | take lenN xs == needle = toInteger i
          | otherwise = go (i + 1) (tail xs)
    loop i maxI na pa found
      | i > maxI = (pa, na, found)
      | found    = (pa, na, found)
      | otherwise =
          let (pa', na', found') = innerLoop 0 i na pa found
          in loop (i + 1) maxI na' pa' found'
    innerLoop j i na pa found
      | j >= i = (pa, na, found)
      | otherwise =
          let rightBound = min (toInteger $ length s) (j + i)
              ss = take (fromInteger $ rightBound - j) $ drop (fromInteger j) s
              ss' = if length ss < fromInteger i
                    then let missing = i - toInteger (length ss)
                             saStart = j - missing
                             sa = if saStart < 0 then "" else take (fromInteger missing) $ drop (fromInteger saStart) s
                             saInt = if null sa then 0 else read sa :: Integer
                             sa2 = show (saInt + 1)
                             zeros = replicate (fromInteger i) '0'
                             combined = zeros ++ sa2
                             suffix = drop (length combined - fromInteger (i - toInteger (length ss))) combined
                         in ss ++ suffix
                    else ss
              n = max 2 (read ss' :: Integer)
              s' = concatMap show [max 1 (n - 1) .. n + lenStr `div` i + 1]
              p = findSubstr s s'
          in if p >= 0
             then if n < na
                  then innerLoop (j + 1) i n (pos (n - 1) + p - toInteger (length $ show (n - 1))) True
                  else innerLoop (j + 1) i na pa True
             else innerLoop (j + 1) i na pa found