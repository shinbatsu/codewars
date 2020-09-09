import Data.List
data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)
isInteresting n l
  | f n = Yes
  | f (n+1) || f (n+2) = Almost
  | otherwise = No
  where
    f x = x > 99 && (any ($ x) checks || elem x l)
    checks =
      [
        isRound,
        isAllSame,
        isPalindrome,
        isIncrementing,
        isDecrementing
      ]
    s x = show x
    isRound a = let str = s a in head str /= '0' && all (== '0') (tail str)
    isAllSame a = length (nub (s a)) == 1
    isPalindrome a = let str = s a in str == reverse str
    isIncrementing a = s a `isInfixOf` "1234567890"
    isDecrementing a = s a `isInfixOf` "9876543210"