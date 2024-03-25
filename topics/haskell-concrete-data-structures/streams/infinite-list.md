# Infinite lists

https://hackage.haskell.org/package/infinite-list-0.1

Haskell's list can be infinite, e.g. `[0..]`, but they are designed as finite lists with a data ctor to specify an empty list. And since it is impossible to determine whether a given list is infinite, it is better to design a new data type, similar to list but without the empty list data ctor.

```hs
-- the only diff is the presence of the Nil ctor in List
data List   a = Nil | a :> List   a
data Stream a =       a :> Stream a
```

Dealing with a stream or an explicitly infinite list means some functions common on lists cannot be defined; e.g. there's no sense in defining
- `length`
- `reverse`
- `last`
- `init`
- `elem` either returns True, or does not terminate, but never returns False
- `maximum`, `minimum`, `sum`, `product` are unlikely to be productive, 
   unless the underlying Ord or Num instance is *extremely lazy*.

These functions cannot be productive on infinite lists:
- `foldr'`
- `foldMap'`
  (because forcing an accumulator even to a WHNF makes fold non-terminating)
- `foldl`, `foldl'`, `foldl1`
  (because left fold cannot be productive)

Altogether it means that code, polymorphic by `Foldable`, cannot confidently work with infinite lists. Even a trivial refactoring can get you in a deep trouble. It's better to save users from this pitfall and do not provide `Foldable` at all. A right fold is fine however.

Since there is no `Foldable`, there could be no `Traversable`. Even if it was not prohibited because of a missing superclass, there are only a few monads, which are lazy enough to be productive for infinite traversals. If you are looking for a traverse with a lazy state, use `mapAccumL`.


These functions may be productive on infinite lists:
- `foldr`, `foldr1`, `foldMap`, `fold`
- `toList`
- `null`


## Laziness

Operations that return a data type with a single ctor can be implemented in an *extremely lazy* fashion - namely, always return the ctor before inspecting any of the arguments.

For example, note the irrefutable pattern matching (~) in `Data.List.NonEmpty`

```hs
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f ~(a :| as) = f a :| fmap f as
```

>Due to the lazy match, forcing the result to WHNF doesn't force any arg.

```hs
Data.List.NonEmpty.map undefined undefined `seq` 1 -- 1
```

This is not the case for normal lists: since there are two data ctors, `map` has to inspect the arg before returning anything, and thus

```hs
Data.List.map undefined undefined `seq` 1 -- throws an error
```

While `Data.List.Infinite` has a single ctor, we believe that following the example of `Data.List.NonEmpty` is harmful for the majority of applications.

Instead, the laziness of the API is modeled on the laziness of respective operations on `Data.List`: a function `Data.List.Infinite.foo` operating over `Infinite a` is expected to have the same strictness properties as `Data.List.foo` operating over [a]. For instance,

```hs
Data.List.Infinite.map undefined undefined `seq` 1 -- diverges
```


## Indexing

Most of historical APIs (such as `Data.List`) use `Int` to index elements of containers. This library makes another choice: namely, indices are represented by an unsigned type, `Word`. 

This way the notorious partial function (!!) becomes total:

```hs
(!!) :: [a] -> Int -> a
-- becomes total
(!!) :: Infinite a -> Word -> a
```

An argument can be made to use an arbitrary-precision type `Natural` instead of finite `Word`. Unfortunately, this causes performance penalties since `Natural` is represented by a heap object and cannot be easily unboxed.

On any GHC-supported architecture, the addressable memory is less than 
`maxBound :: Word` bytes and thus it's impossible to materialize a container with more than `maxBound :: Word` elements.
