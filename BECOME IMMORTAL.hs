elderAge m n l t
  | m == 0 || n == 0 = 0
  | m > n = elderAge n m l t
  | l > ln = 0
  | lm == ln = (rs 1 (ln - l - 1) * (m + n - ln) + elderAge (ln - n) (lm - m) l t) `mod` t
  | otherwise = esc m n l t lm ln
  where
    pred x = until (>= x) (*2) 1
    rs l r = if r < l then 0 else div ((l + r) * (r - l + 1)) 2
    lm = pred m
    ln = pred n
    
esc m n l t lm ln =
  let lm' = div ln 2
      a = rs 1 (ln - l - 1) * m
      b = (ln - n) * rs (max 0 (lm' - l)) (ln - l - 1)
      c = if l <= lm'
            then (lm' - l) * (lm' - m) * (ln - n) + elderAge (lm' - m) (ln - n) 0 t
            else elderAge (lm' - m) (ln - n) (l - lm') t
  in mod (a - b + c)  t
  where
    rs l r = if r < l then 0 else div ((l + r) * (r - l + 1)) 2