import Data.Set(fromList,toList)
permutations = toList . fix (\f s -> if null s then fromList [""] else fromList [x:p | x <- s,p <- toList(f(d x s))]) 
  where fix g = g(fix g); d f [] = []; d y (x:a)| y == x = a | otherwise = x :d y a