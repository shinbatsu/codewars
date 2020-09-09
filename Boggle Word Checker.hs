findWord m (w:ws)=any (\(x,y)->f ws x y [(x,y)]) 
  [ (x,y) | y <- [0..length m - 1], x <- [0..length (head m) - 1], m !! y !! x == w ]
  where
    f [] a b c=True
    f (c:cs) x y v=any (\(a,b)->m !! b !! a == c && notElem (a,b) v && f cs a b ((a,b):v))
      [ 
        (a,b) | a <- [x-1..x+1], b <- [y-1..y+1],
        a >= 0, b >= 0,
        a < length (head m), b < length m,
        (a,b) /= (x,y) 