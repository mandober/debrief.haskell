# Functor type class

- mappable data types
- data types and data structure
- containers and contexts
- containers and contextual data types
- functorial data types
- functorful of `a`'s
- value (single element) vs set of values (elements)
- "...is a functor in the n-th type parameter"
- mapping function
- lifting a mapping function (into a functor context)
- a mapping function from `a` to `b`
  - mapping all elements using the mapping function,
    element-wise mapping, i.e it is apoplied to each element
  - replacing (instead of mapping) all elements with a *constant value*
  - replacing (instead of mapping) all elements with the *unit value*, `()`
- a mapping must
  - must not change *shape* of the data structure (container/context)
  - must not change the *size* of data structure, i.e. number of elements
  - may change the type of elements
  - "shape" subsumes type and size of a data structure
  - a data structure is denoted `f a` - it has the shape `f` which it must retain, but the type of elements, `a`, may vary.
- member types
  - Functor class targets unary type ctors, `f :: * -> *`
  - Types with nullary type ctors are never functors




## Functor class and its members

The `Functor` type class groups together *mappable data types*, i.e. data types that can be mapped.

Each member data type is said to be *functorial* (or mappable) in some way.

Data types, thought of as *data structures* (primarily lists, Sequence, NonEmpty, but also trees, Graph, Map, etc.) are mappable in an obvious way: since they are viewed as *containers* holding one or more elements, we can expect that mapping a container means mapping each element within. 

Some data types that are mappable as well are instead though of as providing a *context*; that is, we say that a value is suspended in some sort of a surrounding context. `IO` is the canonical exmaple of such types, but the function type, Maybe, pair, tuples, Either, etc. may be *contextual* as well.



Mappable data types are primarily thought of as *containers* (list, Maybe, Identity); that is, 




Mapping a data type involves a *mapping functions* (supplied), which is a regular function `a -> b`, i.e it can map an `a` into a `b`; it also involves the data type itself, denoted by `f a`, often pronounced as: "a functorful of `a`'s" (a functor full of `a`'s), which is a data structure consisting of element(s) of type `a`.

## Functorial data types

Pretty much all data types with polyadic (unary and up) type ctors are functors in some way. Types with a nullary type ctor are never functors (Void, (), Bool, Char, Int, Double, etc.) as there is no context, no container.

- Maybe
- Identity
- (->) a b
- (,) a b
- Either e a
- 

For example, the `Maybe a` type is functor since it can be mapped; it is a simpleset data structure, consisting of a single element of type `a` (which, when present, is wrapped in a data ctor `Just`). The simplest functor is actually `Identity a` that just wraps a value of type `a`. Maybe if, after all, more complicated since the value may not be present; nevertheless that doesn't affect it being a functor - when the value is present it is mapped, when the value is absent (i.e. when it is `Nothing`), then `Nothing`, possibly with a different type, is returned. The list is probably the most canonical example of a functor; it even comes with its own mapping functions, `map`, so `fmap` for lists is just `map`.

For some types, however, it is not immediately clear as to how are they functors. A pair `(a, b)`, for example, can certainly be mappped, but the question is which field? As it turns out, a pair is *considered a functor* in its second component; in other words, a pair is *functorial* in its second component. To explian that, consider the type of key-value map, `Map k v`, which is reasonably functorial in the value type `v` - although we can map keys, `k`, that is really not a sensible thing to do (and what would it accomplish?). Similarly, the function type `a -> b` is functorial in the return type `b` - we can alomost consider such function as "nearly a value" - we are just waiting for a key, `a`, to return the associated value `b`. This is one aspect of how functions can be treated as data, most directly as finite maps (key-value finite map data structure). And since a k/v map may be represented as a collection of `(k, v)` pairs, this explains why a pair is considered functorial in second component; another aspect of this is that the `length` function applied to a pair returns 1 (not 2 as someone may reasonably expect). And this also means that we don't need `snd` function to extract the second element from a pair before mapping it - we can directly use `fmap`.






which is a data structure of the shape .

means applying the provided function



## Functor: functions and operators

- Functor methods
  - fmap
  - <$
- minimal class definition: fmap
- Functor associated functions
  - <$>
  - <&>
  - $>
  - void

## ## Functor: description of functions

### fmap

`fmap` takes a function `a -> b` and a data structure `f a` and returns the same data structure but with elements mapped, `f b`.



All auxillary functions are defined in terms of `fmap` and all are convenience functions (not really needed).
- `<$>` is fmap in infix form
- `<&>` is fmap in infix form with args flipped
- `$>`  is fmap without the function, only with its return value instead
- `<$`  is flipped `$>`
- `void` is fmap without the function, with a constant value instead


## Functor class

```hs
class Functor f where
  -- | Mapping
  fmap :: (a -> b) -> f a -> f b

  -- | Replace all locations in the input with the same value.
  (<$) :: a -> f b -> f a
  (<$) = fmap . const
  infixl 4 <$

  {-# MINIMAL fmap #-}
```

## Functor: additional methods

```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
infixl 4 <$>

-- | Flipped version of (<$)
($>) :: Functor f => f a -> b -> f b
($>) = flip (fmap . const)
infixl 4 $>

-- | Flipped version of fmap
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

-- | Discards or ignores the result of evaluation
void :: Functor f => f a -> f ()
void fa = fmap (\_ -> ()) fa
-- i.e.
void fa = fmap (const ()) fa
-- or
void fa = () <$ fa
-- or
void fa = (\_ -> ()) <$> fa
```
