import Data.List; import Numeric

part n =
  let x = sort . nub . map product $ f n 1
      t = div (length x) 2
      m = if odd (length x) then fromIntegral (x !! t)
               else (fromIntegral (x !! (t-1)) + fromIntegral (x !! t)) / 2
      g = fromIntegral (sum x) / fromIntegral (length x)
  in "Range: " ++ show (last x - head x)
     ++ " Average: " ++ showFFloat (Just 2) g ""
     ++ " Median: " ++ showFFloat (Just 2) m ""
     
f a b | a < b = [] | otherwise = [[a]] ++ [i : xs | i <- [b..a-1], xs <- f (a - i) i]