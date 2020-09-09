import Data.List (intercalate)

solution :: [Integer] -> String
solution = intercalate "," . ext
  where
    ext :: [Integer] -> [String]
    ext [] = []
    ext (x:xs) = fl x x xs
    
    fl:: Integer -> Integer -> [Integer] -> [String]
    fl start prev [] =
      [join start prev]
    fl start prev (a:as)
      | a == prev + 1 = fl start a as
      | otherwise = join start prev : fl a a as
    
    join :: Integer -> Integer -> String
    join a b
      | b - a >= 2 = show a ++ "-" ++ show b
      | b - a == 1 = show a ++ "," ++ show b
      | otherwise  = show a