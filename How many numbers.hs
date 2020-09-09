findAll :: Int -> Int -> (Int, Maybe Int, Maybe Int)
findAll d a = 
  let f a 0 = [[]]
      f a n = [ y:t | (i,y) <- zip [0..] a, t <- f (drop i a) (n-1)]
      g = read . concatMap show
      b = [ g c | c <- f [1..9] a, sum c == d ]
  in case b of
       [] -> (0, Nothing, Nothing); a -> (length a, Just (minimum a), Just (maximum a))