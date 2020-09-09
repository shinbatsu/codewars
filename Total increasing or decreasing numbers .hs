totalIncDec :: Integer -> Integer
totalIncDec x=foldl(\n i->n*(x+i)`div`i)1[1..9]*(x+20)`div`10-1-10*x