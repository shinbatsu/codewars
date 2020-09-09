spiralize 1 = [[1]]
spiralize 2 = [[1,1],[0,1]]
spiralize n = [ if i>0 && i<=l then (f i . r) i else r i | i <- [0..n-1]]
  where
    l = div n 2 - if mod n 4 == 0 
      then 1 
      else 0
    r i = [ if even (minimum [i,j,n-1 - max i j]) 
      then 1 
      else 0 | j <- [0..n-1]]
    f i row = take (i-1) row ++ [1 - row !! (i-1)] ++ drop i row