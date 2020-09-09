import Data.Char
multiply a b = show . sum . map (\(c,d) -> read 
  (show (n * toInteger d) ++ replicate c '0')) $ zip [0..] (reverse $ map digitToInt b)
    where n = read a