import Data.List; import Data.Maybe (fromJust)
decode [] _ = []
decode a n = (!! n) $ iterate step (replicate (length a) []) !! length a
  where step b = sort $ zipWith (:) a b
encode [] = ([],0)
encode a = (map last v, fromJust $ elemIndex a v)
  where v = sort (take (length a) (iterate f a))
        f (y:b) = b ++ [y]; f [] = []