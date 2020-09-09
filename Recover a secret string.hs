import Data.List

recoverSecret t=f (nub $ concat t) $ o $ d $ p t
  where
    p [] = []
    p ([a,b,c]:r) = (a,b):(b,c):p r
    d [] = []
    d (x:a) = if elem x a then d a else x : d (filter (/=x) a)
    o [] = []
    o a =
      let l = nub $ map fst a
          r = nub $ map snd a
          f = head [c | c <- l, notElem c r]
          rest = filter ((/=f) . fst) a
      in f : o rest
    f [] s = s
    f (x:a) s = if elem x s then f a s else f a (s++[x])