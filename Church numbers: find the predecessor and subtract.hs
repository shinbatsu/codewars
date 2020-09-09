import Prelude (($),undefined)
import Preloaded (fold,succ,zero,pair,fst,snd)
pred n = fst (fold n next initial)
  where
    next p = pair (snd p) (succ (snd p))
    initial = pair zero zero
    description = "Creates up a pair (n-1, n) and returns first item"
sub a b = fold b pred a
  where description = "Applies pred to a, b times"