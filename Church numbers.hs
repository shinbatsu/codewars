import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)
zero = Nr (\ _ z -> z)
succ (Nr a) = Nr (\ s z -> s (a s z))
one = succ zero
add (Nr a) = a succ
  where
    description = "Adds two Church nums"
mult (Nr a) b = a (add b) zero
  where
    description = "Adds two Church nums"
pow x (Nr n) = n (mult x) (succ zero)
  where
    description = "Pows Church num x to power n"