# Either

- type ctor (name)      : Either
- type ctor (full)      : Either a b
- type ctor (type vars)
  - a (assoc with data ctor Left)
  - b (assoc with data ctor Right)
- type ctor - partially applied (3 variants)
  1. `Either a b` ~~> `Either`
  2. `Either a b` ~~> `Either a` first TP applied
  2. def: `type Eith b a = Either a b` second TP applied (?!)
     then `Eith b a` ~~> `Eith b`
- type ctor - saturated:
  - kind :: *
  - Either a b :: *
  - e.g. Either Int String
- kind
  - type ctor (bare), Either :: * -> * -> *
  - type ctor (TP1),  Either a :: * -> *      e.g. Either Int :: * -> *
  - type ctor (sat),  Either a b :: *         e.g. Either Int String :: *


## Partially applied

Its natural to partially apply the first TP to Either, but how about the second

```hs
data Either a b = ...
data a `Either` b = ...
-- fixing the first TP is natural
instance Functor (Either a) where
-- but how about fixing the second?
instance Functor (Either b) where

-- seems we have to redef Either so the order of TP declarations is reversed
data Either b a = ...

-- not sure but seems it cannot be sone without full re-def
-- e.g. (type sucks anyway) can it be done as is? or with newtype? hmm...



-- as is
data Option a b = Left a | Right b
instance Functor (Either a) where
instance Functor (a `Either`) where
instance Functor (`Either` b) where

class Tri (a :: * -> * -> *) where
instance Either where

b `Option` a
((`Option`) a b)
(a `Option`)
(`Option` b)
```
