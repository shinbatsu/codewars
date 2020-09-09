import Data.List
justify s z = intercalate "\n" $ f (words s)
  where
    f [] = []
    f a = let (l,r) = t a [] in
      (if null r then unwords else jl) l : f r
    t (y:b) a | length (unwords (a++[y])) <= z = t b (a++[y])
    t b a = (a, b);t [] a = (a, [])
    jl [w] = w;jl a =
      let t = z - sum (map length a)
          g = length a - 1
          (x,e) = divMod t g
          sp = replicate e (replicate (x+1) ' ')
            ++ replicate (g - e) (replicate x ' ')
      in concat $ zipWith (++) a (sp ++ [""])