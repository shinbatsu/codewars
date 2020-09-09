import Data.List (find)

cut :: String -> [String]
cut s = reverse $ go c sz []
  where
    c = lines s
    totalO = length $ filter (=='o') $ concat c
    rows = length c
    cols = length (head c)
    sz = (rows*cols) `div` totalO
    go cake size acc = case findCoord (/='x') cake of
      Nothing -> acc
      Just (y,x) -> trySlices (y,x)
      where
        trySlices (y,x) = ho [] [ r
          | w <- [size,size-1..1]
          , h <- [1..size]
          , w*h == size
          , let slice = getSlice cake x y w h
          , valid slice
          , let r = go (cutOut cake x y w h) size (unlines slice : acc)
          , not (null r)
          ]
    getSlice cake x y w h = map (take w . drop x) $ take h $ drop y cake
    valid s = case foldl check (0, True) (concat s) of
      (1, True) -> True
      _ -> False
      where
        check (oCount,noX) c = (oCount + if c == 'o' then 1 else 0, noX && c /= 'x')
    cutOut cake x y w h =
      [ [ if inRect j i then 'x' else ch | (j,ch) <- zip [0..] row ]
      | (i,row) <- zip [0..] cake ]
      where inRect j i = i >= y && i < y + h && j >= x && j < x + w
    findCoord p cake = go 0 cake
      where
        go _ [] = Nothing
        go y (r:rs) = case findIndex p r of
          Just x  -> Just (y,x)
          Nothing -> go (y+1) rs
    findIndex p = go 0
      where
        go _ [] = Nothing
        go i (x:xs) | p x = Just i | otherwise = go (i+1) xs
    ho def [] = def
    ho _ (x:_) = x