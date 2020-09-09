import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where return=Identity;(Identity v)>>=f=f v
instance Monad Maybe where return=Just;Nothing>>=x=Nothing;(Just v)>>=f=f v
instance Monad (State s)where return v=State(\s->(v,s));(State g)>>=f=State(\s->let(a,e)=g s;State h=f a in h e)
instance Monad (Reader s)where return v=Reader(const v);(Reader g)>>=f=Reader(\s->runReader(f(g s))s)
instance Monoid w=>Monad(Writer w)where return v=Writer(mempty,v);(Writer(w,v))>>=f=let Writer(t,g)=f v in Writer(w<>t,g)