import Kata.DecodeMorseAdvanced.Preload
decodeBits b =
  let
    t = r b '0'; u = mn t
    d b = if head b == '0' then if length b == u*7 then "   " 
      else if length b == u*3 then " " else if length b == u then "" else "" 
      else if length b == u*3 then "-" else if length b == u then "." else ""
    g [] = []; g (x:xs) = let (h, t) = span (== x) xs in (x:h) : g t
    r s c = let f = dropWhile (== c) . reverse in f (f s)
    mn = minimum . map length . g
  in concatMap d (g t)
decodeMorse c =
  let
    s l [] = [[]]; s l (x:xs)
      | p l (x:xs) = [] : s l (drop (length l) (x:xs))
      | otherwise = let (y:ys) = s l xs in (x:y) : ys
    p [] _ = True; p _ [] = False
    p (x:xs) (y:ys) = x == y && p xs ys
  in unwords . filter (/= "") . map (concatMap (morseCodes!) . words) $ s "   " c