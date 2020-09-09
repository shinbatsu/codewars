pathFinder m = f [[0,0]] g where
  g = lines m; l = length g
  f [] g = g !! (l-1) !! (l-1) == 'Y'
  f ([x,y]:ps) g
    | g !! y !! x /= '.' = f ps g
    | otherwise = f (ps ++ ns) (upd y x 'Y' g)
    where
      ns = filter (\[i,j] -> i >= 0 && j >= 0 && i < l && j < l) [[x+1,y],[x,y+1],[x-1,y],[x,y-1]]
  upd i j v g = take i g ++ [take j (g !! i) ++ [v] ++ drop (j+1) (g !! i)] ++ drop (i+1) g