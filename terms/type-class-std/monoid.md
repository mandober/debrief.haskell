# Monoid

A **monoid** is an algebraic structure made of 1 associative binary operation with identity closed over the carrier set (monoid = semigroup + identity).

For example, lists form a monoid under concatenation, functions (from a set onto itself) under composition. In category theory, the automorphisms form a monoid (conversely, a monoid may be viewed as a category with a single object).


## Monid class

Monoid is a subclass of semigroup. Monoid class has a binary function that obeys the axioms of identity and associativity.

```hs
-- Defined in GHC.Base
class (Semigroup a) => Monoid a where
  mempty  :: a              -- identity
  mappend :: a -> a -> a    -- join, infix: <>
  mconcat :: [a] -> a       -- flat join

instance Monoid ()
instance Monoid Ordering
instance Monoid [a]

instance (Monoid a)                               => Monoid (IO a)
instance (Monoid b)                               => Monoid (a -> b)
instance (Semigroup a)                            => Monoid (Maybe a)
instance (Monoid a, Monoid b)                     => Monoid (a, b)
instance (Monoid a, Monoid b, Monoid c)           => Monoid (a, b, c)
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d)
```

Monoid typeclass requires you to define 3 functions on your type so it can be a member of the Monoid class:
- `mempty`  defines the identity element (minimal requirement)
- `mappend` defines joining elements together
- `mconcat` defines joining and flattening of elements


```hs
-- empty list is a list identity
[] = mempty

-- mappend = binary op to append (join) args
mappend [1..5] []
mappend [] [1..5]
mappend [1,2] [3..6]

-- mconcat is a binary op to join and flatten the args
mconcat [[1..3], [4..6]]   -- [1,2,3,4,5,6]

-- more generally
mappend x mempty = x
mappend mempty x = x
```

## Functor

- `Functor` class is used for types that can be mapped over.
- Instances: `[]`, `Maybe`, `Either a`, `IO`, `(->) r`, `(,) a`
- note how `Either a` is an instance and not just `Either`, i.e. it has the 


Instances of `Functor` should satisfy the following laws:
- `fmap id == id`
- `fmap (f . g) == fmap f . fmap g`
- The instances of Functor for lists, Data.Maybe.Maybe and System.IO.IO
satisfy these laws.


```hs
-- in GHC.Base
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b -- <$>

  -- Replace all locations in the input with the same value.
  -- The default definition: fmap . const
  -- may be overridden with a more efficient version.
  (<$) :: a -> f b -> f a
  (<$) = fmap . const
```




Function `fmap` or `<$>`:
- arg1, `a -> b`, is a function that receives the `a` from arg2,     
  and returns a (possibly) different type `b`
- arg2, `f a`, is a `Functor f` that takes a type arg `a`.     
  That is, the `f` is a type that has an instance of the `Functor` typeclass.
- return value is `f b`. It is the same `f` from `f a`,     
  while the type arg `b` may possibly refer to a different type (then `a`).

Function `<$`:
- the first arg is a type, denoted by the type param `a`



List is an instance of Functor so we can call `map` as `fmap`:

Below, the segmented function, `(+3)`, fits the functors' required signature for *arg1*: `a -> b`; it will receive each element from the list (arg2) of type `a`, manipulate it in some way, and return the result as a (possibly) different type `b`. Then `fmap` will *repack* that return value of type `b` into `f b`. As for the arg2, the list of ints will do fine considering the required signature `f a` for arg2. The type of any list is `[a]` which can be desugared into `a : []` i.e. `: a []`. So `:` matches `f` as a value constructor.

```hs
(+3) <$> [2,4,6]  -- [5,7,9]  fmap

3 <$     [2,4,6]  -- [3,3,3]  half-fmap?
'a' <$   [2,4,6]  -- "aaa"    half-fmap?


(+3) <$> (Right 5)  -- Right 8
it :: Num b => Either a b

(+3) <$> (Left 5)   -- Left 5
it :: (Num b, Num a) => Either a b
```
