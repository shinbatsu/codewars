import qualified Data.Array as A
import Data.Array; import Data.List

d=[(x,y)|x<-[-1..1],y<-[-1..1],(x,y)/=(0,0)]
g a b=case compare(length a)(length b)of GT->a;LT->b;EQ->min a b

longestPath s=
  let r=lines s;h=length r;w=length(head r);b=((0,0),(h-1,w-1))
      res=foldl' g "" [c!p|p<-A.indices a]
      a=A.array b [((x,y),(r!!x)!!y)|x<-[0..h-1],y<-[0..w-1]]
      c=A.array b [(p,f p)|p<-A.indices a]
      f p= let (x,y)=p;u=a!p; ns=[(x+dx,y+dy)|(dx,dy)<-d,A.inRange b (x+dx,y+dy),a!(x+dx,y+dy)>u]; q=foldl' g ""[c!n|n<-ns] in u:q
  in res