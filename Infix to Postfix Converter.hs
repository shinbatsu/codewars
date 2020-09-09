toPostfix= n []"" where
  p '('=0;p '+'=1;p '-'=1;p '*'=2;p '/'=2;p '^'=3; p x=0
  f '^'= True; f x= False
  n s o []= o ++ s; n s o (c:r)
    | c `elem` ['0'..'9']= n s (o ++ [c]) r
    | c== '('= n ('(': s) o r
    | c== ')'= let (x,y:z)= span (/= '(') s in n z (o ++ x) r
    | otherwise= let
        q x= x /= '(' && (p x > p c || (p x==p c && not (f c)))
        (x,y)= span q s
      in n (c:y)(o++x)r