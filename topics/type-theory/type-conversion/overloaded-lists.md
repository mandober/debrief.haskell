# Overloaded Lists

The `IsList` class is defined in the module `GHC.IsList`
- https://hackage.haskell.org/package/base/docs/GHC-IsList.html
- https://hackage.haskell.org/package/base/docs/src/GHC.IsList.html


```hs
class IsList l where
  type Item l

  toList    :: l -> [Item l]
  fromList  :: [Item l] -> l

  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList
```

expoanded

```hs
-- defined in GHC.IsList
class IsList l where
  -- 'Item' type function returns the type of items of the structure 'l'
  type Item l

  -- toList extracts a list of 'Item l' from the structure 'l'.
  toList :: l -> [Item l]

  -- fromList constructs the structure 'l' from the given list of 'Item l'
  fromList  :: [Item l] -> l

  -- LAW 1: fromList . toList = id
  -- LAW 2: toList . fromList = id


  -- fromListN takes the input list's length and potentially uses it to construct the structure 'l' more efficiently compared to 'fromList'. (If the given number does not equal to the input list's length the behaviour of 'fromListN' is not specified)
  --
  -- >>> fromListN (length xs) xs == fromList xs
  --
  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList
```

The `IsList` class and its methods are intended to be used in conjunction with the *OverloadedLists* extension. When enabled, the types that have the `IsList` instance can also use the internal list construct, `[]`, both at type and term level.

1. Without the OverloadedLists pragma

For example, a key-value mapping of integers to strings, `Map Int String`, can be stated as a literal associative list, `[(Int, String)]`, but it requires one more operation to become a map: `M.fromList` must be applied to it.

```hs
import Data.Map (Map)
import Data.Map qualified as M

kv :: Map Int String
kv = M.fromList [(1, "one")]
```

It might not seem as a big concern, but it's no fun constantly sprinkling the code with `M.fromList`. Moreover, showing maps during the GHCi experiments, only to see each one prefixed with `fromList […]`, gets annoying pretty fast. However, the pragma won't fix that, but only a suitable `Show` instance.

2. With the OverloadedLists pragma

With the pragma enabled, the types with the `IsList` instance are allowed to use the (previously GHC-reserved) list construct `[]`.

```hs
{-# LANGUAGE OverloadedLists #-}

import Data.Map (Map)
import Data.Map qualified as M

-- M.fromList may be elided:
kv,qv :: Map Int String
kv = [(1, "one")]
-- but redundancy is fine as well:
qv = M.fromList [(1, "one")]

-- in any case GHCi prints: M.fromList […]
```

**Practically**, this means we can ditch the `M.fromList` calls; although, we need not to since redundant `M.fromList` calls are fine. Only `M.fromList` calls placed in front of the literal (list) construction may be elided; and only in case there's an appropriate type signature to relay the exact type (of something that was previously a list literal exclusively). That is, the explicit conversion into a map with `M.fromList` is still required (despite the "mappy" type signature) for expression that return a list, e.g. `zip`.

```hs
indexed :: (Ord k, Num k, Enum k) => [a] -> Map k a
indexed xs = M.fromList $ zip [0..] xs
```

**Operationally**, it means that, e.g. `Map Int String`, becomes equipped its own form of a literal (that looks awfully like an associative list literal).

One concrete advantage of this approach: without `T.fromList` (where `T` is an applicable type, such as `[]`, `Map`, `Set`, `Vector`), the type signature is "fluid" - a concrete signature must be stated, but it can be easily swapped for another appropriate one. This way you can try out different types of containers before settling on one.


is that, without a signature, a "list" literal is *polymorphic*. Eventually, you will have to assign it a type, but in the meantime, the list literal is a truly polymorphic container, allowing you to experiment with many applicable container types before you settle on the final one.


```hs agda
import Data.Set (Set)
import Data.Set qualified as S

import Data.Map (Map)
import Data.Map qualified as M

import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.Array (Array)
import Data.Array qualified as A

-- container can be any of
container = M.fromList [(1, "one")]


V.fromList :: forall a. [a] -> Vector a
S.fromList :: forall a. Ord a => [a] -> Set a
M.fromList :: forall k a. Ord k => [(k, a)] -> Map k a
```
