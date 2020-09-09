import Control.Monad.ST; import Data.Array.ST; import Control.Monad
cutLog p n=runST$do
 r<-newArray(0,n)0::ST s(STArray s Int Int)
 forM_[1..n]$ \j ->do
  c<-forM[0..j-1]$ \i->do
   let v=chk p(i+1)
   y<-readArray r(j-i-1)
   return(v+y)
  writeArray r j(maximum c)
 readArray r n
 where chk a i=if i<length a then a!!i else 0