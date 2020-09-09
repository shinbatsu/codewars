import Data.List
exchangeSort :: [Int] -> Int
exchangeSort v=max a b where
  (a,b)=foldl(\(x,y)(f,g)->(x+fromEnum(f<g),y+fromEnum(f>g)))(0,0)$zip v(sort v)