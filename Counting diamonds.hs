import Data.List; import Data.Ord
countDiamonds u t=
  let n= length u
      m= length (head u)
      s= [[sum [u !! y !! x | y <- [0..i], x <- [0..j]] | j <- [0..m-1]] | i <- [0..n-1]]
      g y x | y < 0 || x < 0= 0
             | otherwise= s !! y !! x
      r= [((i-y+1)*(j-x+1),(y,x),(i,j)) |
            i <- [0..n-1], j <- [0..m-1], y <- [0..i], x <- [0..j],
            g i j - g i (x-1) - g (y-1) j + g (y-1) (x-1)== t]
  in if null r then [] else let z = minimum (map (\(sz,n,nn) -> sz) r)
          in map (\(n,a,b) -> (a,b)) $ filter (\(sz,n,nn) -> sz == z) (sortOn (\(sz,(y,x),nn)->(sz,y,x)) r)