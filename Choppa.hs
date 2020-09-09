import Data.Sequence (Seq(..), (><), singleton, fromList, empty)
import qualified Data.Map as Map

type Pos = (Int, Int)
data Node = Passable | NotPassable deriving (Eq, Show)
type Grid = [[Node]]
type Path = [Pos]

shortestPath g s e = b (singleton s) (Map.singleton s s)
  where
    l = length g; h = length (head g)
    f (x,y) = if y >= 0 && y < l && x >= 0 && x < h then (g !! y) !! x else NotPassable
    n (x,y) = [p | p <- [(x,y+1),(x+1,y),(x,y-1),(x-1,y)], f p == Passable]
    b q v
      | q == empty = []
      | otherwise = case q of
          (c :<| d) ->
            if c == e then reverse (r v e)
            else
              let m = filter (`Map.notMember` v) (n c)
                  w = foldl (\a x -> Map.insert x c a) v m
                  z = d >< fromList m
              in b z w
    r m p | p == s = [p] | otherwise = p : r m (m Map.! p)