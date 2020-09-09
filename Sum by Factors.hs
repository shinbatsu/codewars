import Data.List (sort)
import qualified Data.Map.Strict as M

sumOfDivided xs = sort . M.toList $ foldl (\m x -> foldl (
  \m' p -> M.insertWith (+) p x m') m (f (abs x) 2 [])) M.empty xs
  where
    f x d acc | d*d > x = if x > 1 && notElem x acc then x:acc else acc
              | x `mod` d == 0 = f (g x d) (if d==2 then 3 else d+2) (if elem d acc then acc else d:acc)
              | otherwise = f x (if d==2 then 3 else d+2) acc
    g x d | x `mod` d == 0 = g (div x d) d
          | otherwise = x