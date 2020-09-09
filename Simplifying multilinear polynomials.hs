insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys
  
sortList [] = []
sortList (x:xs) = insert x (sortList xs)

sortTerms [] = []
sortTerms (x:xs) = insertTerm x (sortTerms xs)

insertTerm x [] = [x]
insertTerm x@(cx, vx) (y@(cy, vy):ys)
  | length vx < length vy = x : y : ys
  | length vx > length vy = y : insertTerm x ys
  | vx <= vy              = x : y : ys
  | otherwise             = y : insertTerm x ys

simplify :: String -> String
simplify s =
  let
    extract [] = []
    extract (c:cs)
      | c == '+' || c == '-' =
          let (term, rest) = span (\x -> x /= '+' && x /= '-') cs
          in (c : term) : extract rest
      | otherwise =
          let (term, rest) = span (\x -> x /= '+' && x /= '-') (c:cs)
          in term : extract rest
    parseTerm t =
      let (sign, rest) = case t of
            ('-':xs) -> (-1, xs)
            ('+':xs) -> (1, xs)
            _        -> (1, t)
          (digits, vars) = span (`elem` ['0'..'9']) rest
          coef = if null digits then 1 else read digits
      in (sign * coef, sortList vars)
    collectTerms [] = []
    collectTerms ((c, v):xs) =
      let (same, rest) = span (\(_, v2) -> v2 == v) xs
          total = c + sum [c' | (c', _) <- same]
      in if total /= 0 then (total, v) : collectTerms rest else collectTerms rest
    format (1, v)  = '+' : v
    format (-1, v) = '-' : v
    format (n, v)  = (if n > 0 then '+' : show n else show n) ++ v
    clean [] = []
    clean ('+':'1':x:xs) | x `elem` ['a'..'z'] = '+' : x : xs
    clean ('-':'1':x:xs) | x `elem` ['a'..'z'] = '-' : x : xs
    clean (x:xs) = x : clean xs
    dropLeadingPlus ('+':xs) = xs
    dropLeadingPlus xs = xs
    terms = extract s
    parsed = map parseTerm terms
    sorted = sortTerms parsed
    grouped = collectTerms sorted
    result = concatMap format grouped
  in dropLeadingPlus (clean result)