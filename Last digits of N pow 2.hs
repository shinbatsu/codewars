import Data.List

suff x=mod(x^2)(10^d)==x where d=length(show x)
rec x=Just(x,next) where next=head[w|m<-[1..4],ds<-gen m,let w=ds*10^(length(show x))+x,w>x,suff w]
gen 0=[0];gen m=[d*10^(m-1)+r|d<-[0..9],r<-gen(m-1)]
bind xs ys=case(xs,ys)of ([],x)->ys; (x,[])->xs;(x:t,y:v)->case x<y of True->x:bind t (y:v);False->y:bind(x:t) v
green=((1:bind(unfoldr rec 5)(unfoldr rec 6))!!).pred
  where f x=Just(x,t) where t=head[w|m<-[1..4],ds<-gen m,let w=ds*10^(length(show x))+x,w>x,suff w]