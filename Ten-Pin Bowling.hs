atoi :: Char -> Int
atoi c = fromEnum c - fromEnum '0'

bonus :: String -> Int
bonus ('X':y:_) = 10 + if y == 'X' then 10 else atoi y
bonus (a:'/':_) = 10
bonus (a:b:_) = atoi a + atoi b
bonus (a:_) = atoi a
bonus _ = 0

parse :: Char -> String -> Int
parse _ ('X':_) = 10
parse _ (a:_) = atoi a

bowlingScore :: String -> Int
bowlingScore input = step (filter (/= ' ') input) 0 0
  where
    step _ 10 res = res
    step ('X':xs) n res = step xs (n+1) (res + 10 + bonus xs)
    step (a:'/':xs) n res = step xs (n+1) (res + 10 + parse a xs)
    step (a:b:xs) n res = step xs (n+1) (res + atoi a + atoi b)
    step _ _ res = res