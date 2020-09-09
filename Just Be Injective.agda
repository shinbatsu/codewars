{-# OPTIONS --cubical --safe #-}
open import Cubical.Core.Everything renaming (_≡_ to _==_)
open import Maybe

variable A : Set
unwrap : A → maybe A → A
unwrap x v with v
... | just y = y
... | nothing = x

just-injective : ∀ {A : Set} (x y : A) → just x == just y → x == y
just-injective x y proof z = unwrap x t
  where t = proof z