module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where
import Control.Monad.State

type ST = State ([Integer]) ()
data V = Var Int | Lit Integer

as o a b = state $ \e -> ((), case a of Lit _ -> e; Var i -> take i e ++ [o (g a e) (g b e)] ++ drop (i+1) e) 
  where g (Lit x) _ = x; g (Var j) e = e !! j

def r = let (v,e) = runState r [] in g v e where g (Lit x) _ = x; g (Var i) e = e !! i
var v = state $ \s -> (Var (length s), s ++ [v])
lit = Lit
while r f a = state $ fix $ \l e ->
  let v = case r of Lit x -> x; Var i -> e !! i
  in if f v then l (execState a e) else ((),e)
  
(+=), (-=), (*=) :: V -> V -> ST
(+=) = as (+)
(-=) = as (-)
(*=) = as (*)