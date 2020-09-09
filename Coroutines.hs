import Control.Monad ( ap,forever)
import Preloaded

apply::Coroutine r u d a ->(Command r u d a -> r)-> r
apply(Coroutine f)k = f k
instance Applicative (Coroutine r u d) where
  pure  = return
  (<*>) = ap
(>>>) ::Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
(>>>) p1 (Coroutine p2) = Coroutine $ \k -> 
  p2 (\c->
    case c of
      Done a -> k (Done a)
      Out d c' -> k (Out d (p1 >>> c'))
      In f -> apply (pipe2 p1 f) k
  )

pipe2 ::Coroutine r u m a ->(m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 (Coroutine p1) f = Coroutine $ \k ->
  p1 (\c->
    case c of
      Out d c' -> apply (c' >>> f d) k
      In g -> k (In (\u ->pipe2 (g u) f))
      Done a -> k (Done a)
  )
instance Monad (Coroutine r u d) where
  return x = Coroutine (\k ->k(Done x))
  (>>=) (Coroutine f) g= Coroutine $ \k ->
    f (\c->
      case c of
        Out d c'-> k (Out d (c' >>= g))
        In h -> k (In (\u -> h u >>= g))
        Done a ->apply (g a) k
    )

output :: a  -> Coroutine r u a ()
output v = Coroutine $ \k -> k (Out v (return ()))
input :: Coroutine r v d v
input = Coroutine $ \k -> k (In return)
produce :: [a] -> Coroutine r u a ()
produce [] = return ()
produce (x:xs) = output x >> produce xs
consume :: Coroutine [t] u t a -> [t]
consume c = runCoroutine c step
  where
    step (Out v c') = v : consume c'
    step _= []
    
filterC p = forever $ do
  x <- input
  if p x then output x else return ()
limit :: Int -> Coroutine r v v ()
limit v = if v <= 0 then return ()
          else input >>= output >> limit (v - 1)
suppress :: Int -> Coroutine r v v ()
suppress 0 = forever $ input >>= output
suppress n = input >> suppress (n - 1)
add:: Coroutine r Int Int ()
add = forever $ do
  a <- input
  b <-input
  output (a + b)
duplicate :: Coroutine r v v ()
duplicate = forever $ Coroutine $ \k ->
  input `apply` \cmd -> case cmd of
    Done a -> k (Done a)
    In f -> k (In $ \u -> output u >> output u)

p1, p2, p3, p4 :: Coroutine r Int Int ()
p1 = filterC even >>> limit 5
p2 = produce (scanl1 (+) [1..])
p3 = forever $ do
  x <- input
  output (x * 2)
p4 = Coroutine $ \cont -> 
  let
    iter lst = Coroutine $ \c' -> do
      c' $ In $ \v -> 
        let next = iter v
            oC = Coroutine $ \z -> z (Out (lst + v) next)
        in oC
  in cont (In iter)