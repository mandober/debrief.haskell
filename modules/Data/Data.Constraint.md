# Data.Constraint

- package: `constraints`
- package-version: `constraints-0.13`
- https://hackage.haskell.org/package/constraints-0.13/docs/Data-Constraint.html
- module: `Data.Constraint`

```hs
(&&&) :: (a :- b) -> (a :- c) -> a :- (b, c)
(***) :: (a :- b) -> (c :- d) -> (a, c) :- (b, d)


refl        :: a :- a
contract    :: a :- (a, a)
trans       :: (b :- c) -> (a :- b) -> a :- c
implied     :: (a => b) => a :- b

weaken1     :: (a, b) :- a
weaken2     :: (a, b) :- b
strengthen1 :: Dict b -> (a :- c) -> a :- (b, c)
strengthen2 :: Dict b -> (a :- c) -> a :- (c, b)

top         :: a :- (() :: Constraint)
bottom      :: Bottom :- a

(\\)        :: HasDict c e => (c => r) -> e -> r
mapDict     :: (a :- b) -> Dict a -> Dict b
unmapDict   :: (Dict a -> Dict b) -> a :- b
withDict    :: HasDict c e => e -> (c => r) -> r

type role (:-) nominal nominal
type (:-) :: Constraint -> Constraint -> *
newtype (:-) a b = Sub (a => Dict b)

type (:=>) :: Constraint -> Constraint -> Constraint
class (:=>) b h | h -> b where
  ins :: b :- h
  {-# MINIMAL ins #-}

type Bottom :: Constraint
class GHC.Types.Any => Bottom where
  no :: a
  {-# MINIMAL no #-}

type Class :: Constraint -> Constraint -> Constraint
class Class b h | h -> b where
  cls :: h :- b
  {-# MINIMAL cls #-}

type role Dict nominal
type Dict :: Constraint -> *
data Dict a where
  Dict :: a => Dict a

type HasDict :: Constraint -> * -> Constraint
class HasDict c e | e -> c where
  evidence :: e -> Dict c
  {-# MINIMAL evidence #-}


type (⊢) :: Constraint -> Constraint -> *
type (⊢) = (:-) :: Constraint -> Constraint -> *
type Constraint :: *
data Constraint
```
