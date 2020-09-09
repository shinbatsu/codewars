rectangleRot :: Int -> Int -> Int
rectangleRot a b =
  let
    a' = fromIntegral a / sqrt 2
    b' = fromIntegral b / sqrt 2
    floorA = floor a'
    floorB = floor b'
    r = (floorA + 1) * (floorB + 1) + floorA * floorB
  in
    r + (r `mod` 2) - 1