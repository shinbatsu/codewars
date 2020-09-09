sumOfSquares n
  | s^2==n=1
  | mod(a*4+b)0b1000==7=4
  | any(\i->let t=n-i^2;r=floor.sqrt$fromIntegral t in r^2==t)[1..s]=2
  | otherwise=3
  where s=floor.sqrt$fromIntegral n;(a,b)=until((>0).snd)(\(x,v)->divMod x 4)(n,0)