# Tuples

- tuple is a type that allows storing multiple heterogeneous values (called components) within a single value
- tuples are the usual means of expressing an anonymous product type value
- the number of components in a tuple is called arity
- arity is set in the type and immutable
- type of tuple depends on type of each component and their number
- tuples are referenced by the number of their components:
  - 0-tuple or nullary tuple is its own type called **unit**, `() :: ()`
  - 1-tuple doesn't exists, degenarates into scalar, `(True) :: Bool`
  - 2-tuple or binary tuple is a pair, `(5, 'c') :: (Int, Char)`
  - 3-tuple or ternarytuple is a triple or triplet
  - n-tuple is n-ary
- tuple type ctor, `(,)`, that is a binary type ctor `(,) a b`
- tuple data ctor, `(,)`, that is a binary data ctor `(,) a b`



## Constructors
Tuples have a distinctive builtin syntax (parens and comma) that is used as both, *type ctor* and *data ctor*: `(,)`

```hs
data (,) a b = (,) a b
```

Tuple's type constructor is `(,)`; it is both type ctor and data ctor:

```hs
-- as data constructor
> :t (,)
(,) :: a -> b -> (a, b)

-- as type constructor
> :k (,)
(,) :: * -> * -> *
```


## Syntactic sugar
Tuples are denoted using syntactic sugar with the elements in parenthesis:

```hs
-- desugared form:
> (,) 5 6
(5,6)
it :: (Num b, Num a) => (a, b)

-- sugared form:
> (2,'c')
(2,'c')
(2,'c') :: Num t => (t, Char)
```



## Partially applied type ctor

Like any n-ary ctor (where n > 1) tuple's type ctor can be partiall applied:

```hs
> partialTuple = ((,) 5)
pt :: Num a => b -> (a, b)

> partialTuple 'c'
(5,'c')
it :: Num a => (a, Char)
```




## Info about a tuple type constructor

```hs
> :i (,)

data (,) a b = (,) a b -- Defined in GHC.Tuple
instance (Eq      a, Eq      b) => Eq      (a, b) -- Defined in GHC.Classes
instance (Ord     a, Ord     b) => Ord     (a, b) -- Defined in GHC.Classes
instance (Read    a, Read    b) => Read    (a, b) -- Defined in GHC.Read
instance (Show    a, Show    b) => Show    (a, b) -- Defined in GHC.Show
instance (Bounded a, Bounded b) => Bounded (a, b) -- Defined in GHC.Enum
instance (Monoid  a, Monoid  b) => Monoid  (a, b) -- Defined in GHC.Base
instance Monoid a => Monad       ((,) a)      -- Defined in GHC.Base
instance Monoid a => Applicative ((,) a)      -- Defined in GHC.Base
instance Functor     ((,) a)              -- Defined in GHC.Base
instance Foldable    ((,) a)              -- Defined in Data.Foldable
instance Traversable ((,) a)              -- Defined in Data.Traversable
```
