# Semigroup

In algebra, a **semigroup** is a set together with a binary operation that upholds the axioms of: closure and associativity. 


## Semigroup definition

```hs
class Semigroup a where
    (<>) :: a -> a -> a         -- associative operation
    sconcat :: NonEmpty a -> a
    stimes :: Integral b => b -> a -> a


    -- Reduce a non-empty list with <>
    -- default definition:
    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as where
        go b (c:cs) = b <> go c cs
        go b []     = b

    -- Repeat a value n times.
    -- By making this a member of the class, idempotent semigroups and
    -- monoids can upgrade this to execute in O(1) by picking respectively:
    --      stimes = stimesIdempotent
    --      stimes = stimesIdempotentMonoid
    stimes :: Integral b => b -> a -> a
    stimes = stimesDefault
```

## Semigroup instances

True Semigroup (no constraints)
* unit
* list i.e. string, `[a]`
* Ordering
* Either: `Either a b`

Semigroup if wrapping a Semigroup:
* IO: `IO a` if `a` is a Semigroup
* Maybe: `Maybe a` if `a` is a Semigroup
* tuples if all components are Semigroups
* functions that return a Semigroup


```hs
-- Defined in ‘GHC.Base’
class Semigroup a where
    (<>) :: a -> a -> a
    sconcat :: NonEmpty a -> a
    stimes :: Integral b => b -> a -> a
-- MINIMAL: (<>)

-- true Semigroups
instance Semigroup ()
instance Semigroup [a]
instance Semigroup Ordering
instance Semigroup (Either a b)       -- Defined in ‘Data.Either’

-- Semigroup if wrapping a Semigroup
instance Semigroup b => Semigroup (a -> b)
instance Semigroup a => Semigroup (IO a)
instance Semigroup a => Semigroup (Maybe a)
instance (Semigroup a, Semigroup b) => Semigroup (a, b)
```


## List

```hs
instance Semigroup [a] where
    stimes = stimesList
    (<>) = (++)
```


## NonEmpty

```hs
instance Semigroup (NonEmpty a) where
    (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)
```

## Functions

```hs
instance Semigroup b => Semigroup (a -> b) where
    f <> g = \x -> f x <> g x
    stimes n f e = stimes n (f e)
```
