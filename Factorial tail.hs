import Data.List
zeroes :: Integral a => a -> a -> a
zeroes b n=minimum[ div (z b n)(fromIntegral c) | (b, c) <- r b]
  where
    g 1 x=[];g m b
      | b^2>m=[m|m>1]
      | mod m b==0=b:g(div m b)b
      | otherwise=g m (b+1)
    r x=map(\a->(head a,length a)).group$g x 0b10
    z b 0=0; z b x=div x b + z b (div x b)