data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any character
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
            deriving (Show, Eq)

parseRegExp = \s -> (case p 2 s of Just (r,"") -> Just r; v -> Nothing) 
  where
    p i s = do (r,xs) <- c s; l i r xs
    t r a ('*':xs) = g (ZeroOrMore r:a) xs
    t r a xs = g (r:a) xs
    f [] v = Nothing
    f [x] r = Just (x,r)
    f xs r = Just (Str (reverse xs),r)
    l n a ('|':xs) | n<3 = do (b,ys)<-c xs; l (n+1) (Or a b) ys
    l v a r = Just (a,r)
    c s = g [] s
    g a [] = f a []
    g a (')':xs) = f a (')':xs)
    g a ('|':xs) = f a ('|':xs)
    g a ('(':xs) = do (r,y)<-p 2 xs; case y of ')':zs->t r a zs; v ->Nothing
    g a ('.':xs) = t Any a xs
    g v ('*':_) = Nothing
    g a (x:xs)
      | x `elem` "()*|." = Nothing
      | otherwise = t (Normal x) a xs