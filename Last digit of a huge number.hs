import Data.Array (listArray, (!), Array)

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit lst = foldr v 1 lst `mod` 10
  where
    v base exp =
      base ^ (if exp < 4 then exp else (exp `mod` 4 + 4))