import Data.Vector.Unboxed as V (Vector, length, unsafeIndex)

triangle :: V.Vector Char -> Char
triangle r="BRG"!! mod (f r (V.length r) 0 `mod` 3 + 3) 3
  where
    f r a b
      | a==1=fromEnum (V.unsafeIndex r b)
      | otherwise=combMod (-f r (a-c) b-f r (a-c) (b+c))
      where
        c=3^(floor $ logBase 3 $ fromIntegral (a - 1))
    combMod x=mod(x `mod` 3 + 3)3