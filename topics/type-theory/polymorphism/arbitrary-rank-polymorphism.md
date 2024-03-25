# Arbitrary-rank polymorphism

Hierarchy:
- type system
  - polymorphism
    - higher-rank polymorphism
      - rank-n types (arbitrary-rank types)

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html

Terms:
- universal quantification
- arbitrary-rank explicit universal quantification
- higher-rank types
- universally quantified type, forall-type, type scheme
- rank-1 types
- rank-2 types
- rank-n types

## Arbitrary-rank polymorphism: Summary

- `RankNTypes` enables higher-rank types, allows types of arbitrary rank
  - since: 6.8.1
  - implies: ExplicitForAll
  - Rank2Types is deprecated; now only an alias of RankNTypes
- `RankNTypes` enables arbitrary-rank explicit universal quantification in types
  - forall-type aka type scheme
  - forall-types have a rank according to their nesting
  - can nest foralls arbitrarily deep in function arrows
  - can include context
- The Rank is the nesting depth of a forall type
  - rank-1 types: the usual implicitly universally quantified type
  - rank-2 types: the `forall` is on the left of a function arrow
  - rank-3 types: has a rank-2 type on the left of a function arrow
  - rank-n types: `n` can be an arbitrary rank
- In data/newtype decls, ctor args may be polymorphic types of any rank
- By default, GHC won't instantiate type vars to a polymorphic type
- which is the main characteristic of the *impredicative polymorphism*


> **The rank** of a forall-type is its nesting level, counted as the number of function arrows to which it appears on the left.

> By default GHC instantiates polymorphic type variables to monomorphic types.

> **Impredicative polymorphism** allows instantiation of polymorphic type variables with polymorphic types.

> **Subsumption** is a phenomenon when a polymorphic type argument gets instantiated and then re-generalised to match the expected type. However, subsumption happens only at the top level of a type. It won't happend if the foralls are underneath an arrow; although in such case, it may be possible to regain a successful type-check with the help of an eta-expansion.

> GHC performs **implicit quantification** for rigid types, at the outermost level, if there's no explicit `forall`: it finds all the type variables mentioned in the type signature, which are not already in scope, and universally quantifies them.



## Arbitrary-rank polymorphism: Details

All the following types are legal:

```hs
-- (1) rank-1
f1 :: forall a b. a -> b -> a
g1 :: forall a b. (Ord a, Eq b) => a -> b -> a

-- (2) rank-2
f2 :: (forall a. a -> a) -> Int -> Int
g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int

-- (3) rank-3
f3 :: ((forall a. a -> a) -> Int) -> Bool -> Bool

-- (4)
-- RankNTypes is required when:
-- forall is to the right of the arrow:
f5 :: Int -> forall a. a -> a
-- class context is to the right of the arrow:
g5 :: Int -> Ord a => a -> a
-- forall is to the left and right of the arrow:
f4 :: Int -> (forall a. a -> a)
```

1. f1 and g1 are *rank-1 types*, and can be written in standard Haskell (e.g. without forall, as `f1 :: a -> b -> a`). The forall makes explicit the universal quantification that is implicitly added.

2. The functions f2 and g2 have *rank-2 types*; **the forall is on the left of a function arrow**. As g2 shows, the polymorphic type on the left of the function arrow can be overloaded.

3. The function f3 has a *rank-3 type*; it has a rank-2 types on the left of a function arrow.


The language option `RankNTypes` enables higher-rank types. That is, you can nest forall types arbitrarily deep in function arrows.

A forall-type (type scheme), including a type-class context, is legal if:
- it appears on the LHS or RHS of function arrows (e.g. `f4`)
- as the arg of a ctor, or type of a field, in a data type declaration (any of f1, f2, f3, g1, g2 above would be valid field type signatures)
- as the type of an *implicit parameter*
- in a *pattern type signature*

The `RankNTypes` is also required for any type with a `forall` or context to the right of an arrow (`f5` or `g5`). Such types are technically *rank-1*, but are clearly not Haskell-98, and an extra extension did not seem worth the bother.

## Type declarations

In particular, in `data` and `newtype` declarations the constructor args may be polymorphic types of any rank.

> Note that the declared types are nevertheless always monomorphic. This is important because by default GHC will not instantiate type variables to a polymorphic type (impredicative polymorphism).

```hs
-- Type declarations whose data ctors have polymorphic arg types:

data T a = T1 (forall b. b -> b -> b) a
-- T1 data ctors has rank-2 type:
-- T1 :: forall a. (forall b. b -> b -> b) -> a -> T a


data MonadT m = MkMonad
  { return :: forall a. a -> m a
  , bind   :: forall a b. m a -> (a -> m b) -> m b
  }
-- MkMonad data ctors has rank-2 type:
-- MkMonad :: forall m. (forall a. a -> m a)
--                   -> (forall a b. m a -> (a -> m b) -> m b)
--                   -> MonadT m


newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])
-- MkSwizzle data ctors has rank-2 type:
-- MkSwizzle :: (forall a. Ord a => [a] -> [a]) -> Swizzle

-- ----------------------------------------------------------------------------
-- Construct these values by applying the constructor to values as usual:

a1 :: T Int
a1 = T1 (\ x y -> x) 3

a2, a3 :: Swizzle
a2 = MkSwizzle sort
a3 = MkSwizzle reverse

a4 :: MonadT Maybe
a4 = let r x   = Just x
         b m k = case m of
            Just y -> k y
            Nothing -> Nothing
     in  MkMonad r b

mkTs :: (forall b. b -> b -> b) -> a -> a -> [T a]
mkTs f x y = [T1 f x, T1 f y]
```

As usual, the type of arg can be more general than the type required, as `MkSwizzle reverse` shows (`reverse` doesn't need the `Ord` constraint).

When you use pattern matching, the bound variables may now have polymorphic types.

```hs
f :: T a -> a -> (a, Char)
f (T1 w k) x = (w k x, w 'c' 'd')

g :: (Ord a, Ord b) => Swizzle -> [a] -> (a -> b) -> [b]
g (MkSwizzle s) xs f = s (map f (s xs))

h :: MonadT m -> [m a] -> m [a]
h m [] = return m []
h m (x:xs) = bind m x          $ \y ->
             bind m (h m xs)   $ \ys ->
             return m (y:ys)
```

In the function `h` we use the record selectors `return` and `bind` to extract the polymorphic bind and return functions from the `MonadT` data structure, rather than using pattern matching.


## Subsumption

> **Subsumption** is the phenomenon when a type argument gets instantiated and then re-generalised to match the expected type.

Suppose:

```hs
f1 :: (forall a b. Int -> a -> b -> b) -> Bool
g1 ::  forall x y. Int -> y -> x -> x

-- :type (fox gox) :: Bool

f2 :: (forall a. (Eq a, Show a) => a -> a) -> Bool
g2 ::  forall x. (Show x, Eq x) => Int -> a -> b -> b  -- type error
```

then `f1 g1` and `f2 g2` are both well typed, despite the different order of type variables and constraints. What happens is that *the argument is instantiated, and then re-generalised to match the type expected by the function*.

However, this instantiation and re-generalisation happens only at the top level of a type. In particular, none of this happens if the foralls are underneath an arrow.

```hs
f3 :: (Int -> forall a b. a -> b -> b) -> Bool
g3a :: Int -> forall x y. x -> y -> y
g3b :: forall x. Int -> forall y. x -> y -> y
g3c :: Int -> forall x y. y -> x -> x

f4 :: (Int -> forall a. (Eq a, Show a) => a -> a) -> Bool
g4 ::  Int -> forall x. (Show x, Eq x) => x -> x) -> Bool
```

- the application `f3 g3a` is well-typed, because `g3a` has a type that matches the type expected by `f3`.

But `f3 g3b` is not well typed, because the foralls are in different places.

Nor is `f3 g3c`, where the foralls are in the same place but the variables are in a different order.

Similarly `f4 g4` is not well typed, because the constraints appear in a different order.

These examples **can be made to typecheck by eta-expansion**:
- `f3 (\x -> g3b x)` is well typed, and similarly 
- `f3 (\x -> g3c x)` and 
- `f4 (\x -> g4 x)`


A similar phenomenon occurs for *operator sections*. For example, 
`(g3a "hello")` is not well typed (!), but it can be made to typecheck by eta expanding it to `\x -> x g3a "hello"`.


HISTORICAL NOTE: Earlier versions of GHC allowed these now-rejected applications, by inserting automatic eta-expansions, as described in Section 4.6 of [Practical type inference for arbitrary-rank types](https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/), where it is called *"deep skolemisation"*, but these automatic eta-expansions may silently change the semantics of the user's program, and deep skolemisation was removed from the language by [GHC Proposal #287](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst). This proposal has many more examples.



## Type inference

**In general, type inference for arbitrary-rank types is undecidable.**

GHC uses an algorithm proposed by Odersky and Laufer: `Putting type annotations to work` from POPL'96, to get a decidable algorithm by requiring some help from the programmer. We do not yet have a formal specification of "some help" but the rule is this:

> For a *lambda-bound or case-bound variable*, `x`, either the programmer provides an explicit polymorphic type for `x`, or the type inference will assume that `x`'s type has no `forall`s in it.

Supplying the explicit type for `x`:

1. You can do that by giving a type signature for `x` directly, using a pattern type signature (see *lexically scoped type variables*), thus:

```hs
(\ (f :: (forall a. a -> a)) -> (f True, f 'c'))
```

2. Alternatively, you can give a type signature to the enclosing context, which GHC can "push down" to find the type for the variable:

```hs
(\ f -> (f True, f 'c')) :: (forall a. a -> a) -> (Bool, Char)
```

Here the type signature on the expression can be pushed inwards to give a type signature for `f`.

3. Similarly, and more commonly, one can give a type signature for the function itself:

```hs
h :: (forall a. a->a) -> (Bool,Char)
h f = (f True, f 'c')
```

4. You don't need to give a type signature if the lambda bound variable is a constructor argument. Here is an example we saw earlier:

```hs
f :: T a -> a -> (a, Char)
f (T1 w k) x = (w k x, w 'c' 'd')
```

Here we do not need to give a type signature to `w`, because it is an argument of constructor `T1` and that tells GHC all it needs to know.


## Implicit quantification

> GHC performs **implicit quantification** as follows: only at the outermost level, of user-written types, iff there is no explicit `forall`: GHC finds all the type variables mentioned in the type that are not already in scope, and universally quantifies them.

For example, the following pairs are equivalent:

```hs
f :: a -> a
-- means:
f :: forall a. a -> a

g (x::a) = let
              h :: a -> b -> b
              h x y = y
           in __
-- means:
g (x::a) = let
              h :: forall b. a -> b -> b
              h x y = y
           in __
```

Notice that GHC always adds implicit quantifiers _at the outermost level_ of a user-written type; it does _not_ find the inner-most possible quantification point. For example:

```hs
f :: (a -> a) -> Int
         -- MEANS
f :: forall a. (a -> a) -> Int
         -- NOT
f :: (forall a. a -> a) -> Int


g :: (Ord a => a -> a) -> Int
         -- MEANS
g :: forall a. (Ord a => a -> a) -> Int
         -- NOT
g :: (forall a. Ord a => a -> a) -> Int
```

If you want the latter type, you can write your `forall`s explicitly. Indeed, doing so is strongly advised for rank-2 types.

Sometimes there's no "outermost level", in which case no implicit quantification happens:

```hs
data PackMap a b s t = PackMap (Monad f => (a -> f b) -> s -> f t)
```

This is rejected because there is no "outermost level" for the types on the RHS (it would obviously be terrible to add extra parameters to `PackMap`), so no implicit quantification happens, and the declaration is rejected (with "`f` is out of scope"). Solution: use an explicit `forall`:

```hs
data PackMap a b s t = PackMap (forall f. Monad f => (a -> f b) -> s -> f t)
```
