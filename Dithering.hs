dithering w h = dith d
  where
    d = ceiling (logBase 2 (fromIntegral (max w h)))
    dith 0 = [(0,0)]
    dith n = concatMap expand (dith (n - 1))
      where
        border = [(0,0), (p, p), (p,0), (0, p)]
          where p = 2^(n - 1)
        expand (x,y) = filter inside [ (x + dx, y + dy) | (dx, dy) <- border ]
        inside (x,y) = x < w && y < h