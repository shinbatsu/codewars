import Data.Char (isDigit, isSpace)
calc s = let
  ws = filter (not . isSpace) s
  next ('-':xs) = let (n,r) = next xs in (-n,r)
  next xs = case span (\c->isDigit c||c=='.') xs of
    ("",_) -> error "Err"
    (n,r) -> (read n,r)
  factor ('(':xs) = let (v,r) = expr xs in case r of
    (')':rs) -> (v,rs)
    _ -> error "Err"
  factor xs@(c:_)
    | c=='-' = let (v,r) = factor (tail xs) in (-v,r)
    | c=='(' = factor xs
    | otherwise = next xs
  term xs = let (f,r) = factor xs in step f r where
    step acc ('*':ys) = let (f,r) = factor ys in step (acc*f) r
    step acc ('/':ys) = let (f,r) = factor ys in step (acc/f) r
    step acc rs = (acc,rs)
  expr xs = let (t,r) = term xs in step t r where
    step acc ('+':ys) = let (t,r) = term ys in step (acc+t) r
    step acc ('-':ys) = let (t,r) = term ys in step (acc-t) r
    step acc rs = (acc,rs)
  (v,r) = expr ws
  in if null r then v else error "Err"