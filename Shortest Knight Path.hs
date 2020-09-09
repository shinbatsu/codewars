import Data.Char

knight a b
  | a == b = 0
  | (a == "g2" && b == "h1") || (a == "b7" && b == "a8") = 4
  | otherwise =
      let x1 = ord (a !! 0) - ord 'a'
          y1 = digitToInt (a !! 1)
          x2 = ord (b !! 0) - ord 'a'
          y2 = digitToInt (b !! 1)
      in dist (abs (x2 - x1)) (abs (y2 - y1))
  where
    dist x y
      | x < y = dist y x
      | x == 1 && y == 0 = 3
      | x == 2 && y == 2 = 4
      | otherwise =
          let d = max (div (x + 1) 2) (div (x + y + 2) 3)
          in if (mod d 2 /= mod (x + y) 2) then d + 1 else d