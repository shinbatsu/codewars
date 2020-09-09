import Data.Set (Set, fromList, intersection, member, map, toList)
import Data.Foldable

getGeneration a 0 = a
getGeneration a g = f g (ac a)
  where
    f 0 s = tg s
    f n s = let b = br s; t = ng s b in f (n-1) t
    
ac a = fromList [(x,y) | (y,l) <- zip [0..] a, (x,c) <- zip [0..] l, c /= 0]
nb (x,y) = fromList [(x+i,y+j) | i <- [-1..1], j <- [-1..1]]

br s
  | null s = (0,0,0,0)
  | otherwise = (z, l, u, v)
  where
    xs = Data.Set.map fst s; ys = Data.Set.map snd s
    z = minimum xs; l = minimum ys; u = maximum xs; v = maximum ys

ng a (z, l, u, v) = fromList
  [ 
  (x,y) | x <- [z-1 .. u+1],
  y <- [l-1 .. v+1],
  let c = length (intersection a (nb (x,y))),
  c > 2 && c < 4 + if member (x,y) a then 1 else 0
  ]

tg s
  | null s = []
  | otherwise = [[if member (x,y) s then 1 else 0 | x <- [z .. u]] | y <- [l .. v]]
  where (z, l, u, v) = br s