import Data.List
middlePermutation s|c==1=b:x:r(t++d)|1>0=x:r(t++b:d)where(a,b:d)=splitAt e$sort s;(e,c)=divMod(length s)2;(x,t)=f a;f [z]=(z,[]);f(z:v)=let(l,m)=f v in(l,z:m);r=reverse