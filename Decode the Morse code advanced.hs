import Haskell.Codewars.MorseDecoder.Preloaded (morseCode)
import Data.Maybe

decodeBitsAdvanced b =
  let
    v (a,b) = if a /= '0' then [".","-","-"] !! f b else [""," ","   "] !! f b
    r s c = let f = dropWhile (== c) . reverse in f (f s); t = r b '0'
    g [] = []; g (a:as) = let (h,t) = span (== a) as in (a,length h + 1) : g t
    s = if l < 6 then 0x11 * (minimum (map snd k)) * l else 10 * sum (map snd k)
    f n = if n * 0x12 * l > 5 * s then 2 else if n * 9 * l > s then 1 else 0x0
    k = g t; l = length k
  in concatMap v k
  
decodeMorse c =
  let
    s l [] = [[]]; s l (a:as)
      | p l (a:as) = [] : s l (drop (length l) (a:as))
      | otherwise = let (b:bs) = s l as in (a:b) : bs
    p [] _ = True; p _ [] = False; p (a:as) (b:bs) = a == b && p as bs
  in unwords . filter (/= "") . map (concatMap (\k -> fromMaybe "" (lookup k morseCode)) . words) $ s "   " c
