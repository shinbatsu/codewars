import qualified Data.Map as M
import Data.Char

v (a,b) = [(a-1,b),(a+1,b)]
h (a,b) = [(a,b-1),(a,b+1)]

line m =
  let g = M.fromList [((a,b),v) | (a,row) <- zip [0..] m, (b,v) <- zip [0..] row, not (isSpace v)]
  
      xs = [k | (k,v) <- M.toList g, v == 'X']
      
      moves d path p =
        let (a,b) = p in case d of
          '+' -> if length path > 1 && fst (head (tail path)) == a then v p else h p
          '|' -> v p
          '-' -> h p
          'X' -> h p ++ v p
          t -> []
          
      options d path p =
        [x | x <- moves d path p, M.member x g, notElem x path,
         d == '+' || (fst x == fst p && g M.! x /= '|') || (snd x == snd p && g M.! x /= '-')]
         
      step beg end = r [beg]
        where
          r [] = False
          r (p:pathTail)
            | p == end = length (p:pathTail) == M.size g
            | otherwise =
                let d = g M.! p
                    possibles = options d (p:pathTail) p
                in case possibles of
                    [nxt] -> r (nxt:p:pathTail)
                    t -> False
  in case xs of
       [a,b] -> step a b || step b a
       t -> False