import Data.List (intercalate)

formatDuration :: Integer -> String
formatDuration 0 = "now"
formatDuration s = join . build s $ units
  where
    units = [(31536000, "year"), (86400, "day"), (3600, "hour"), (60, "minute"), (1, "second")]

    build :: Integer -> [(Integer, String)] -> [String]
    build 0 _ = []
    build s [] = []
    build s ((unit, name):rest)
      | count > 0 = format count name : build remainder rest
      | otherwise = build s rest
      where
        count = s `div` unit
        remainder = s `mod` unit

    format 1 name = "1 " ++ name
    format n name = show n ++ " " ++ name ++ "s"

    join [] = ""
    join [x] = x
    join [x, y] = x ++ " and " ++ y
    join xs = intercalate ", " (init xs) ++ " and " ++ last xs