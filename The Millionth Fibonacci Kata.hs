fib :: Integer -> Integer
fib n = (if n < 0 then sign n else id) $ iter 1 0 0 1 (abs n)
  where
    sign x = if even x then negate else id
    iter _ b _ _ 0 = b
    iter a b p q n
      | even n    = iter a b (p*p + q*q) (2*p*q + q*q) (div n 2)
      | otherwise = iter (b*q + a*q + a*p) (b*p + a*q) p q (n - 1)