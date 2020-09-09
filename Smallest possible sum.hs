import Data.List
smallestPossibleSum a=fromIntegral(length a)*foldr1 gcd a