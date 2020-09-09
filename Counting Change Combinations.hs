countChange t a = case (t, a) of
  (0, x) -> 1
  (x, []) -> 0
  (n, x:b) | n<0 -> 0
    | otherwise -> countChange (n-x) (x:b) + countChange n b