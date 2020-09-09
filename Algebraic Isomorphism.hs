import Data.Void

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst
substR :: ISO a b -> (b -> a)
substR = snd
isoBool :: ISO Bool Bool
isoBool = (id, id)
isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)
refl :: ISO a a
refl = (id, id)
symm :: ISO a b -> ISO b a
symm (a, b) = (b, a)
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))
isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)
isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)
isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (either (Left . ab) (Right . cd), either (Left . ba) (Right . dc))
isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) =  (\f -> cd . f . ba, \f -> dc . f . ab)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab, ba) = (f ab, f ba)
  where
    f g a = case g (Just a) of
      Just b  -> b
      Nothing -> case g Nothing of
        Just b  -> b
        Nothing -> error "Err"
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (Left . either (() :) (const []), from)
  where
    from (Left []) = Right ()
    from (Left (_:xs)) = Left xs
    from (Right v) = absurd v
isoSymm = (swapPair, swapPair)
  where swapPair (f, g) = (g, f)

-- Sometimes, we can treat a Type as a Number:
-- if a Type t has n distinct value, it's Number is n.
-- This is formally called cardinality.
-- See https://en.wikipedia.org/wiki/Cardinality

-- Void has cardinality of 0 (we will abbreviate it Void is 0).
-- () is 1.
-- Bool is 2.
-- Maybe a is 1 + a.
-- We will be using peano arithmetic so we will write it as S a.
-- https://en.wikipedia.org/wiki/Peano_axioms
-- Either a b is a + b.
-- (a, b) is a * b.
-- a -> b is b ^ a. Try counting (() -> Bool) and (Bool -> ())

-- Algebraic data type got the name because
-- it satisfies a lot of algebraic rules under isomorphism

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (f, f) where
  f (Left  x) = Right x
  f (Right x) = Left  x

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (left, right)
  where
    right :: Either a (Either b c) -> Either (Either a b) c
    right (Left a) = Left (Left a)
    right (Right (Left b))  = Left (Right b)
    right (Right (Right c)) = Right c
    
    left :: Either (Either a b) c -> Either a (Either b c)
    left (Left (Left a))  = Left a
    left (Left (Right b)) = Right (Left b)
    left (Right c) = Right (Right c)
    
-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (swap, swap)
  where swap (x, y) = (y, x)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (l, r)
  where
    l ((a, b), c) = (a, (b, c))
    r (a, (b, c)) = ((a, b), c)
    
-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = ( \ (a, e) -> case e of
         Left b  -> Left  (a, b)
         Right c -> Right (a, c)
       , \ e -> case e of
         Left  (a, b) -> (a, Left  b)
         Right (a, c) -> (a, Right c)
       )

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = ( \f (a, b) -> f a b
           , \f a b -> f (a, b) )

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = ( \b -> if b then Just Nothing else Nothing
      , \x -> case x of Just _ -> True; Nothing -> False )

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (l, Right)
  where
    l (Left  x) = absurd x -- absurd :: Void -> a
    l (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = ( \e -> case e of
          Left Nothing -> Nothing
          Left (Just a) -> Just (Left a)
          Right b -> Just (Right b)
        , \m -> case m of
          Nothing -> Left Nothing
          Just (Left a) -> Left (Just a)
          Just (Right b) -> Right b
        )

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (undefined, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (l, r)
  where
    l x = case x of
      (Nothing, b) -> Left b
      (Just a, b) -> Right (a, b)

    r y = case y of
      Left b -> (Nothing, b)
      Right (a, b) -> (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (\f -> (), \f -> absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = ( \f -> (f Nothing, f . Just)
       , \(a, f) b -> case b of
           Nothing -> a
           Just x  -> f x)

-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = ( ($ ()) , const ) --_ `trans` powS `trans` _