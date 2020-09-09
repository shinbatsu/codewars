freakContazSequence sq = snd $ foldr f' (a,b) (init sq)
  where
    f = \x y -> mod (mod x y + y) y
    (a,b) = case last sq of 'D' -> (3,3); 'U' -> (3,4); _ -> (3,2);
    f' c (a,b) = case c of
      'D' -> (a*3,b*3)
      'd' -> let t = b * 3 + 1 in (a*3, div (if odd t then t+a*3 else t) 2)
      _ -> let t = b * 3 - 2
               m = f t 4
               c = if f (a * 3) 4 /= 1 then m else (-m)
               tmp = div (t + (mod (4 + c) 4) * a * 3) 4
               res = if tmp == 1 then tmp + a * 3 else tmp
           in (a * 3, res)