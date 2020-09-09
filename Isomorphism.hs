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
    from (Left [])     = Right ()
    from (Left (_:xs)) = Left xs
    from (Right v)     = absurd v
isoSymm = (swapPair, swapPair)
  where swapPair (f, g) = (g, f)