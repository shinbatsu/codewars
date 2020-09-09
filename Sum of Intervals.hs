import Data.List; import Data.Ord
sumOfIntervals::[(Int,Int)]->Int
sumOfIntervals=f(-maxBound)0.sortBy(fst`comparing`)where f v r[]=r;f m r((a,b):d)=f(max m b)(r+max 0(b-max m a))d