countOnes :: Integer -> Integer -> Integer
countOnes a b|a>b=0|otherwise=f b-f(a-1)where f 0=0;f n=let l=log2 n;b=2^l in div b 2*l+n-b+1+f(n-b);log2 n=if n<2 then 0 else 1+log2(div n 2)