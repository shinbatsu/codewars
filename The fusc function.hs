fusc n= snd $ foldr step (1,0)(f n)
  where
    f 0= []; f x= (mod x 2):f(div x 2)
    step 1 (a,b)= (a, b+a)
    step 0 (a,b)= (a+b, b)