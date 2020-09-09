{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module MicroLens where

import Prelude hiding (sum)
import Data.Monoid
import Control.Applicative
import qualified Data.Traversable as T
import Data.Functor (void)

---------------------------------------------------------
class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap ::  (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap ::  (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f
  
class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
  
instance Profunctor (->) where
  dimap f g h = g . h . f
  
instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)
  
class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

newtype K b a = K { getK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id { getId :: a } deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

---------------------------------------------------------
-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b =
  p a (f b) -> p s (f t)
  
type Iso s t a b =
  forall p f . (Profunctor p, Functor f) =>
  Optic p f s t a b
  
type Lens s t a b =
  forall f . Functor f => 
  Optic (->) f s t a b
  
type Traversal s t a b =
  forall f . Applicative f =>
  Optic (->) f s t a b
  
type Fold s a = 
  forall f . (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f . (Choice p, Applicative f) =>
  Optic p f s t a b
    
---------------------------------------------------------

run :: (a -> k) -> Optic (->) (K k) s s a a -> s -> k
run f o s = getK (o (K . f) s)

-- | A lens focusing on the first element in a pair
_1 :: Lens (a, x) (b, x) a b
_1 f (a,x) = fmap (,x) (f a)

-- | A lens focusing on the second element in a pair
_2 :: Lens (x, a) (x, b) a b
_2 f (x,a) = fmap (x,) (f a)

-- view :: Lens s t a b -> (s -> a)
view :: Optic (->) (K a) s t a b -> (s -> a)
view o s = getK (o K s)

-- over :: Lens s t a b -> (a -> b) -> (s -> t)
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over o f s = getId (o (Id . f) s)

-- set :: Lens s t a b -> b -> (s -> t)
set :: Optic (->) Id s t a b -> b -> (s -> t)
set optic b = over optic (const b)

-- | A traversal which focuses on each element in any 
elements :: T.Traversable f => Traversal (f a) (f b) a b
elements = T.traverse

-- toListOf :: Traversal s s a a -> (s -> [a])
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf o s = appEndo (run (Endo . (:)) o s) []

-- preview :: Traversal s s a a -> (s -> Maybe a)
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview o s = getFirst (run (First . Just) o s)

-- | A helper function which witnesses the fact that any
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

-- | A Fold which views the result of a function application
to :: (a -> b) -> Fold a b
to f g a = fmap (const a) $ g $ f a

-- | A prism which focuses on the left branch of an Either
_Left :: Prism (Either a x) (Either b x) a b
_Left = rmap (either (fmap Left) (pure . Right)) . left'

-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
_Right = rmap (either (pure . Left) (fmap Right)) . right'

_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = dimap (uncurry $ flip (,)) (fmap $ uncurry $ flip (,))