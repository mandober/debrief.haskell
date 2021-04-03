# OverloadedLists


This extension allows using the list notation for construction of structures like Set, Map, IntMap, Vector, Text and Array.

With OverloadedLists enabled, use type/expr annotation or type applications to indicate the desired type of an ambiguous expression.

```hs
['0' .. '9']             :: [Char]
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text



-- normally, this is always a list of chars (no type ambiguity)
['0' .. '9'] :: [Char]
-- with OverloadedLists enabled it may be ambiguous
-- use type annotation to resolve ambiguities:
['0' .. '9'] :: Set Char
-- or type application where appropriate


[0..9] :: Set Int
[0..9] :: Set Integer
[0..9] :: [Integer]
[0..9] :: [Int]
-- all these above produce the same list of values but diff type!
fromList [0,1,2,3,4,5,6,7,8,9]

-- Float produces basically the same Set/List only nums have a dec point
[0..9] :: [Float]
fromList [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
```



List patterns are also overloaded. When the *OverloadedLists* extension is turned on, these patterns:

```hs
f [] = ...
g [x,y,z] = ...
```

are treated as

```hs
f (toList -> []) = ...
g (toList -> [x,y,z]) = ...
```

> During the typechecking and desugaring phases, GHC uses whatever is in scope with the names `fromList`, `toList` and `fromListN`.


*GHC.Exts* module exports the `IsList` class that can be used to overload `fromListN` and `fromListN` for different structures.

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
