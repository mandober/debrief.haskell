# OverloadedLists

https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists

his extension allows programmers to use the list notation for construction of
structures like: Set, Map, IntMap, Vector, Text
and Array. The following code listing gives a few examples:

```hs
['0' .. '9']             :: Set Char
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text
```

List patterns are also overloaded. When the OverloadedLists extension is turned on, the
definitions

f [] = ...
g [x,y,z] = ...

will be treated as

f (toList -> []) = ...
g (toList -> [x,y,z]) = ...


GHC, during the typechecking and desugaring phases, uses whatever is in scope
with the names of fromList, toList and fromListN (i.e., fromList, toList and
fromListN are rebindable).


That said, the *GHC.Exts* module exports the `IsList` class that can be used to overload `fromListN` and `fromListN` for different structures. The type class is defined as follows:

```hs
class IsList l where
  type Item l
  fromList  :: [Item l] -> l
  toList    :: l -> [Item l]

  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList
```

The `IsList` class and its methods are intended to be used in conjunction with the *OverloadedLists* extension.

- The `Item` type function returns the type of items of the structure `l`.
- The `fromList` constructs the structure `l` from the given list of Item `l`.
- The `fromListN` function takes the input list's length as a hint. Its behaviour should be equivalent to `fromList`. The hint can be used for more efficient construction of the structure l compared to fromList. If the given hint is not equal to the input list's length the behaviour of `fromListN` is not specified.

The instances of the IsList class should satisfy the following property:

fromList . toList = id

In the following, we give several example instances of the IsList type class:

```hs
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
