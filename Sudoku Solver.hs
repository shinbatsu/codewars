sudoku m = head $ solver m
  where
    solver b | all (all (/=0)) b = [b]
         | otherwise = 
         [
           r | v <- [1..9], 
           notElem v (row ++col ++ sq), 
           r <- solver (check b i j v)
         ]
      where
        (i,j) = head [(x,y) | x <- [0..8], y <- [0..8], b !! x !! y == 0]
        row = b !! i; col = [b !! x !! j | x <- [0.. 8]]
        
        sq = [b !! r !! c | r <- slice i, c <- slice j]
        slice x = let s = (div x 3)*3 in [s..s+2]
        
        check b x y v = take x b ++[take y (b !! x) ++ [v] ++ drop (y+1) (b !! x)] ++ drop (x+1) b