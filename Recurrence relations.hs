import qualified Data.Map.Strict as M
import Data.Bifunctor

evaluateFunction f a=snd$eval f M.empty a
eval f m a=maybe c(z m)(M.lookup a m)
 where
  z m (Left b)=(m,b)
  z m (Right(j,g))=let(v,n)=foldr(r f)(m,[])j;b=g n in(M.insert a(Left b)v,b)
  c=eval f(M.insert a(f a)m) a
r f v(m,a)=second(:a)(eval f m v)