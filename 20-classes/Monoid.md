# Monoid

In algebra, a **monoid** is a set together with a binary operation that upholds the axioms of closure, associativity and identity.

The pattern of Monoid is outlined as: types that have binary functions that let you join things together in accordance with the laws of associativity, along with an identity value that will return the other argument unmodified.



## Monoid definition

In Haskell, the **Monoid** is a type class. A type whose binary operator or function upholds the axioms of closure, associativity and identity.

Monoid is Semigroup plus the identity element. So to be monoids, types must first be semigroups.

```hs
-- Defined in ‘GHC.Base’
class Semigroup a => Monoid a where
    mempty  :: a
    
    mappend :: a -> a -> a
    -- mappend = (<>)

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty  -- default definition
-- MINIMAL: mempty
```


## Monoid instances

True monoids (no constraints)
* list i.e. string
* unit
* Ordering

Monoids if wrapping a Monoid:
* tuples if all components are Monoids
* IO: `IO a` if `a` is a Monoid
* functions that return a Monoid

Monoid if wrapping a Semigroup:
* Maybe: `Maybe a` if `a` is a Semigroup



```hs
-- true monoids
instance Monoid [a]                             -- list, string
instance Monoid ()                              -- unit
instance Monoid Ordering                        -- Ordering(LT, EQ, GT)

-- Monoid if wrapping a Monoid
instance Monoid a => Monoid (IO a)              -- IO
instance Monoid b => Monoid (a -> b)            -- functions
instance (Monoid a, Monoid b) => Monoid (a, b)  -- tuples

-- Monoid if wrapping a Semigroup
instance Semigroup a => Monoid (Maybe a)        -- Maybe
```


## Ordering as a Monoid

```hs
-- EQ vs LT: LT wins
LT `mappend` EQ         -- LT
EQ `mappend` LT         -- LT

-- EQ vs GT: GT wins
GT `mappend` EQ         -- GT
EQ `mappend` GT         -- GT

-- LT vs GT: LEFT ONE wins
LT `mappend` GT         -- LT
GT `mappend` LT         -- GT

EQ `mappend` EQ         -- EQ
```


## List as a Monoid

List as Monoid is straightforward, just concat the lists (or strings).

```hs
instance Monoid [a] where
    mempty = []
    mappend = (++)
    mconcat xss = [x | xs <- xss, x <- xs]


[1,2,5,6] `mappend` [3,4,7,8]                   -- [1,2,5,6,3,4,7,8]
"abc" `mappend` "def"                           -- "abcdef"
```

## Unit as a Monoid

Unit is a trivial monoid and concat of units just gives a unit.

```hs
instance Monoid () where
    mempty        = ()
    mconcat _     = ()
```


## Functions as a Monoid

Functions whose return value is a Monoid can be composed (concat'ed) as Monoids.

```hs
> :t (\x -> [x]) `mappend` (\y -> [y])
(\x -> [x]) `mappend` (\y -> [y]) :: a -> [a]

((\x -> [x]) LT) `mappend` ((\y -> [y]) EQ)     -- [LT,EQ]
```

## Tuples as a Monoid

Tuples are Monoids provided their components are Monoids.

Two pairs are mappended into one pair by using `mconcat` (i.e. Semigroup's concat operation i.e. `<>` operator) to concat the first components of each pair (as the 1st component is the resulting pair), then the second components (as the 2nd component in result): `(a,b) `mappend` (c,d) = (a <> c , b <> d)`

```hs
instance (Monoid a, Monoid b) => Monoid (a, b) where
    (a,b) `mappend` (c,d) = (a <> c , b <> d)


([2], [4]) `mappend` ([6], [8])                 -- ([2,6],[4,8])
([1,2],[3,4]) `mappend` ([5,6], [7,8])          -- ([1,2,5,6],[3,4,7,8])
(EQ,LT) `mappend` (GT, GT)                      -- (GT,LT)
```


## Maybe as a Monoid

Maybe is a Monoid if its inner type is a Monoid.

The Maybe type has more than two possible Monoids.

To maintain the unique pairing of types and typeclass instances, newtypes are used for `Maybe`, the same as with `Sum` and `Product` monoids for integers.

`First` and `Last` Maybe monoids are like boolean disjunction, but with explicit preference for the leftmost or rightmost "success" in a series of Maybe values. We have to choose because with a Boolean, there is just True or False — it doesn't matter where the True or False values occurred. With Maybe, however, we need to make a decision as to which Just value we'll return if there are multiple successes. `First` and `Last` encode these possibilities.


```hs
-- first returns the first or leftmost non-Nothing value:
> First (Just 1) `mappend` First (Just 2)
First {getFirst = Just 1}

-- last returns the last or rightmost non-Nothing value:
> Last (Just 1) `mappend` Last (Just 2)
Last {getLast = Just 2}

-- both succeed to return something as long as there's at least one Just:
> Last Nothing `mappend` Last (Just 2)
Last {getLast = Just 2}

> First Nothing `mappend` First (Just 2)
First {getFirst = Just 2}

-- neither can return anything if all values are Nothing:
> First Nothing `mappend` First Nothing
First {getFirst = Nothing}

> Last Nothing `mappend` Last Nothing
Last {getLast = Nothing}
```

For the third variant of Maybe as monoid we'll consider not choosing one value out of a set of values, but combining the `a` values contained within the `Maybe a` type.

```hs
Just [2,4] `mappend` Just [6,8]                 -- Just [2,4,6,8]
Just EQ `mappend` Just GT                       -- Just GT
```
