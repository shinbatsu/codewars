import Data.List
import Data.Char

top3 s = take 3 . map head . reverse . sortOn length . group . sort $ ws
  where
    t c = if isAlpha c || c == '\'' then toLower c else ' '
    ws = filter (\w->any (/= '\'') w) $ words $ map t s