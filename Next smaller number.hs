import Data.List; import Data.Maybe

nextSmaller :: Integer -> Maybe Integer
nextSmaller n = do
  let s = show n
  i <- listToMaybe [i | i <- [length s -1,length s -2..1], s!!(i-1) > s!!i]
  let p = s!!(i-1)
  j <- listToMaybe [j | j <- reverse [0..length (drop i s)-1], (drop i s)!!j < p]
  let j' = i+j
      sw = [if k==i-1 then s!!j' else if k==j' then s!!(i-1) else c | (k,c)<-zip [0..] s]
      ret = take i sw ++ reverse (drop i sw)
  if head ret /= '0' then Just (read ret) else Nothing