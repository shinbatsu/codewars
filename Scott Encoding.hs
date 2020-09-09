import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

nill = SList $ \n _ -> n
toPair = \sp -> runPair sp (\a b -> (a, b))
fromPair = \(a, b) -> SPair (\f -> f a b)
fst = \sp -> runPair sp (\a b -> a)
snd = \sp -> runPair sp (\a b -> b)
swap = \sp -> runPair sp (\a b -> fromPair (b, a))
curry = \f a b -> f (fromPair (a, b))
uncurry = \f sp -> runPair sp f

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe a) = a Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe a = SMaybe (\x f -> maybe x f a)
isJust :: SMaybe a -> Bool
isJust (SMaybe a) = a False (const True)
isNothing :: SMaybe a -> Bool
isNothing  = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes xs = runList xs nill ct
  where
    ct a sa = case toMaybe a of
                  Nothing -> catMaybes sa
                  Just x  -> cons x (catMaybes sa)

toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right
fromEither :: Either a b -> SEither a b
fromEither e = SEither $ \f g -> case e of
  Left a  -> f a
  Right b -> g b
isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (const True) (const False)
isRight :: SEither a b -> Bool
isRight (SEither e) = e (const False) (const True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = fromPair . go
  where
    go ls = runList ls (nill, nill) $ \a sa ->
      let (l, r) = go sa in
      case toEither a of
        Left x  -> (cons x l, r)
        Right y -> (l, cons y r)
        
toList :: SList a -> [a]
toList xs = runList xs [] (\a sa -> a : toList sa)
fromList :: [a] -> SList a
fromList [] = nill
fromList (a:xs) = cons a (fromList xs)
cons :: a -> SList a -> SList a
cons a l = SList $ \n f -> f a l

concat :: SList a -> SList a -> SList a
concat a b = runList a b (\x xs -> cons x (concat xs b))
null :: SList a -> Bool
null (SList a) = a True (const (const False))
length :: SList a -> Int
length a = runList a 0 (\_ sa -> 1 + length sa)
map :: (a -> b) -> SList a -> SList b
map f xs = runList xs nill (\a sa -> cons (f a) (map f sa))
zip :: SList a -> SList b -> SList (SPair a b)
zip la lb = runList la nill (\a as -> runList lb nill (\b bs -> cons (fromPair (a, b)) (zip as bs)))
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f = \acc -> \xs -> runList xs acc (\a sa -> foldl f (f acc a) sa)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f = \z -> \xs -> runList xs z (\a sa -> f a (foldr f z sa))
take :: Int -> SList a -> SList a
take = \n xs -> if n <= 0 then nill else runList xs nill (\a sa -> cons a (take (n - 1) sa))