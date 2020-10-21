# OverloadedLists

https://gitlab.haskell.org/ghc/ghc/wikis/overloaded-lists
https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists


*OverloadedLists* extension, since GHC 7.8, allows the list notation brackets to be reused for construction of other structures, such as `Set`, `Map`, `IntMap`, `Vector`, `Text`, `Array`.


## Current Implementation

In Haskell, the list notation is used in 7 ways:

```hs
[]          -- Empty list
[x]         -- x : []
[x,y,z]     -- x : y : z : []
[x .. ]     -- enumFrom x
[x,y ..]    -- enumFromThen x y
[x .. y]    -- enumFromTo x y
[x,y .. z]  -- enumFromThenTo x y z
```

With *OverloadedLists* extension turned on, the 7 notations are desugared as:

```hs
[]          -- fromListN 0 []
[x]         -- fromListN 1 (x : [])
[x,y,z]     -- fromListN 3 (x : y : z : [])
[x .. ]     -- fromList (enumFrom x)
[x,y ..]    -- fromList (enumFromThen x y)
[x .. y]    -- fromList (enumFromTo x y)
[x,y .. z]  -- fromList (enumFromThenTo x y z)
```

*OverloadedLists* extension enables:

```hs
['0' .. '9']             :: Set Char
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text
```

List patterns are also overloaded so when the *OverloadedLists* extension is turned on, the definition
- `f [] = ...`      is treated as `f (toList -> []) = ...`
- `g [x,y,z] = ...` is treated as `g (toList -> [x,y,z]) = ...`


GHC, during the *typechecking* and *desugaring* phases, uses whatever is in scope with the names of `fromList`, `toList` and `fromListN` (i.e. these 3 are rebindable).


## IsList class

That said, the *GHC.Exts* module exports the `IsList` class that can be used to overload `fromListN` and `fromListN` for different structures.

The `IsList` class and its methods are intended to be used in conjunction with the `OverloadedLists` extension.
- `Item` type function returns the type of items of the structure `l`
- `fromList` fn constructs the structure `l` from the given list of `Item l`
- `fromListN` fn takes the input list's length as a hint. Its behaviour should be equivalent to `fromList`. The hint can be used for more efficient construction of the structure `l` compared to `fromList`. If the given hint is not equal to the input list's length the behaviour of `fromListN` is not specified.
- The instances of the `IsList` class should satisfy the following property: `fromList . toList = id`


```hs
-- IsList class:
class IsList l where
  type Item l
  fromList  :: [Item l] -> l
  toList    :: l -> [Item l]
  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList

-- example instances of `IsList` type class
instance IsList [a] where
  type Item [a] = a
  fromList = id
  toList   = id

instance (Ord a) => IsList (Set a) where
  type Item (Set a) = a
  fromList = Set.fromList
  toList   = Set.toList

instance (Ord k) => IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromList = Map.fromList
  toList   = Map.toList

instance IsList (IntMap v) where
  type Item (IntMap v) = (Int,v)
  fromList = IntMap.fromList
  toList   = IntMap.toList

instance IsList Text where
  type Item Text = Char
  fromList = Text.pack
  toList   = Text.unpack

instance IsList (Vector a) where
  type Item (Vector a) = a
  fromList  = Vector.fromList
  toList    = Vector.toList
  fromListN = Vector.fromListN
```

Further GHC improvements/extensions that may benefit *OverloadedLists*


## Defaulting

Currently, the `IsList` class is not accompanied with defaulting rules. Although feasible, not much thought has gone into how to specify the meaning
of the default declarations like: `default ([a])`

## Literal Lists

The current implementation of the OverloadedLists extension can be
improved by handling the lists that are only populated with literals in a
special way. More specifically, the compiler could allocate such lists
statically using a compact representation and allow IsList instances
to take advantage of the compact representation. Equipped with this capability
the OverloadedLists extension will be in a good position to subsume the
OverloadedStrings extension (currently, as a special case, string
literals benefit from statically allocated compact representation).

Somewhat related discussions:
- https://gitlab.haskell.org/ghc/ghc/issues/5218
- http://www.mail-archive.com/haskell-cafe@haskell.org/msg101412.html
- http://www.serpentine.com/blog/2012/09/12/the-case-of-the-mysterious-explosion-in-space/



## Heterogeneous Lists

The *OverloadedLists* extension as, implemented above, would not be able to be used on heterogeneous lists, for example, as implemented below:

```hs
data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: a -> HList xs -> HList (a ': xs)
```

This is a bit disappointing. However, I'm not really sure how you could make this extension support this use case, even if you added some hacks to the IsList class.


## Length-{indexed,observed} Vectors

Currently the extension cannot be used to represent list literals for *length-indexed vectors*, such as

```hs
data Nat = Ze | Su Nat
-- (alternatively, GHC.TypeLits.Nat)

data Vec :: * -> Nat -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)
```

as the length-information is not provided in a suitable way.
