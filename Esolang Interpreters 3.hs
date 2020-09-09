import Data.List

interpreter s n w h = intercalate "\r\n" $ map concat $ e 0 0 0 0 (replicate h (replicate w "0")) (filter (`elem` "nesw*[]") s)
  where
    e t p x y b c
      | t >= n || p < 0 || p >= length c = b
      | v == '[' && (b !! y !! x) == "0" = e t (jf c p 1) x y b c
      | v == ']' && (b !! y !! x) /= "0" = e t (jb c p 1) x y b c
      | v == 'n' = mv p x (mod (y - 1) h) b
      | v == 'e' = mv p (mod (x + 1) w) y b
      | v == 's' = mv p x (mod (y + 1) h) b
      | v == 'w' = mv p (mod (x - 1) w) y b
      | v == '*' = mv p x y (tg b x y)
      | otherwise = e (t + 1) (p + 1) x y b c
      where v = c !! p; mv q nx ny z = e (t + 1) (q + 1) nx ny z c
    tg b x y = [ if j == y then [ if i == x then if c == "0" then "1" else "0" else c | (i,c) <- zip [0..] r ] else r | (j,r) <- zip [0..] b ]
    jf c i l
      | l == 0 = i
      | c !! (i + 1) == '[' = jf c (i + 1) (l + 1)
      | c !! (i + 1) == ']' = jf c (i + 1) (l - 1)
      | otherwise = jf c (i + 1) l
    jb c i l
      | l == 0 = i
      | c !! (i - 1) == ']' = jb c (i - 1) (l + 1)
      | c !! (i - 1) == '[' = jb c (i - 1) (l - 1)
      | otherwise = jb c (i - 1) l