import Data.List; import Data.Function
balancedParens :: Int -> [String]
balancedParens=fix(\f n->if n==0 then[""]else nub[take i s++"()"++drop i s|s<-f(n-1),i<-[0..length s]])