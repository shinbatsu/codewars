{-# LANGUAGE FlexibleInstances, UndecidableInstances, InstanceSigs,ScopedTypeVariables,RankNTypes #-}

import Data.List
type ISO a b = (a -> b, b -> a)
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)
substL :: ISO a b -> (a -> b)
substL = fst
substR :: ISO a b -> (b -> a)
substR = snd
liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) =
  ( \f -> \x y -> ab (f (ba x) (ba y)), 
    \g -> \x y -> ba (g (ab x) (ab y))
  )

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a
  iter :: a -> (a -> a) -> n -> a
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  isoP :: ISO n Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP
instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r
instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r
instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

data Peano = O | S Peano deriving (Show, Eq, Ord)
instance Nat Peano where
  zero = O
  successor = S
  nat a _ O = a
  nat _ f (S b) = f b
  iter a f b = case b of
    O     -> a
    S b'  -> f (iter a f b')
  plus a b = iter b successor a
  minus a b = case (a, b) of
    (_, O)         -> a
    (O, _)         -> O
    (S a', S b')   -> minus a' b'
  mult a b = iter zero (\acc -> plus acc b) a
  pow a b = iter (successor zero) (\acc -> mult acc a) b

instance Nat [()] where
  zero = []
  successor as = () : as
  nat a _ []     = a
  nat _ c (_:as) = c as
  iter a f as = foldl (\acc _ -> f acc) a as
  plus = (++)
  minus as bs = foldl removeOne as bs
    where
      removeOne acc _ = case acc of
        []     -> []
        (_:ts) -> ts
  mult as bs = concatMap (const bs) as
  pow base exps = foldl mult [()] (map (const base) exps)

newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott const
  successor a = Scott (\_ f -> f a)
  nat a f (Scott b) = b a f
  iter a f (Scott b) = b a (f . iter a f)
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow
  
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  zero = Church $ \_ a -> a
  successor (Church a) = Church (\ c b -> c (a c b))
  nat a f (Church c) =
    maybe a f (c step Nothing)
    where
      step Nothing  = Just zero
      step (Just b) = Just (successor b)
  iter a f (Church b) = b f a
  plus (Church a) (Church b) = Church $ \f -> a f . b f
  mult (Church a) (Church b) = Church (a . b)
  pow (Church a) (Church b) = Church (b a)
  minus a b = rm a (toi b)
    where
      zero = Church $ \_ z -> z
      rm a 0 = a
      rm a b = case pc a of
        Nothing -> zero
        Just px -> rm px (b - 1)
      toi (Church b) = b (+1) 0
      pc (Church b) =
        if b (const False) True
          then Nothing
          else Just (Church $ \f a -> b (\c d -> d (c f)) (const a) id)