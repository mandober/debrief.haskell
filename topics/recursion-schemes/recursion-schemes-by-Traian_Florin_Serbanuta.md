# Recursion Schemes in Haskell
a talk by Traian Florin Serbanuta
https://www.youtube.com/watch?v=c-kuW_ZN-3Y

## Abstract

Let's explore the paper "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire", or how basic knowledge of abstract algebra can help you develop patterns for recursion.

Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire
by Erik Meijer, Maarten Fokkinga, Ross Paterson
https://maartenfokkinga.github.io/utwente/mmf91m.pdf


## Lists

There is a constructive (initial) and deconstructive (final) view on lists.

A *constructive (initial) view*: A list of elements of type `a` is either empty, as manifest by the nil `[]` data ctor, or an element of type `a` consed onto a list of type [a] as manifest by the cons data ctor `(:)`. In the algebraic and categoriac view, the *initial algebra* is used to fold data (e.g. a list). The simplest recursion schema for recursion that consumes finite data, sumarizing a data structure is called a **catamorphism**. Catamorphism in terms of lists is `foldr` function. However, catamorphisms need an initial algebra and a fixed (iusing `Fix`) functor.

```hs
data [a] = [] | a : [a]
data List a = Nil | Cons a (List a)

data NonEmpty a = a :| [a]
data NonEmpty a = NCons a (List a)

-- final algebra, coalgebra
type Algebra f a = f a -> a

-- fixpoint for functors
newtype Fix f = Fix { unFix :: f (Fix f) }

-- catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . (fmap cata alg) . unFix
```

A *deconstructive (final) view*: A potentially infinite list of elements of type [a] can *maybe* be decomposed into its head of type `a` and its tail of type [a]. This view is realized as a function `unfoldr` which takes a seed value of type `b` and a function that generates the elements of the list from the seed. The generator function returns a pair `(a, b)`, where `a` is a new list element and `b` is the new seed for the next cycle of *corecursion*. The produced list is a potentially infinite *codata*. In the algebraic and categoriac view, the final algebra is used to generate codata (e.g. a list). The simplest recursion schema for corecursion that produces codata is called a **anamorphism**. However, anaamorphisms need an *final algebra* (aka coalgebra) and a fixed (using `Fix`) functor.

```hs
-- if list is guaranteed to be non-empty
-- b is the seed
unfold :: (b -> (a, b)) -> b -> NonEmpty a

-- if list is potentially empty
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

ana :: (b -> (a, b)) -> b -> [a]
ana coalg seed₀ =
  let (elem, seed) = coalg seed₀
  in  elem : ana coalg seed
```

## Cata and ana

Algebra and coalgebra define the logic to be done at a single level of an expression tree. The `fmap` applies that logic to child nodes in the expression tree.

```hs
-- | algebra
type Algebra f a = f a -> a

-- | coalgebra
type Coalgebra f a = a -> f a

-- | fixpoint for functors
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- | anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
```

## F-algebras

Given a functor `F`, the type of F-(co)algebras induced by `F` is

```hs
data ListF a r = Nil | Cons a r
  deriving Functor

data TreeF a r = Empty | Node a r r
  deriving Functor

data ExpF r
  = Num Int
  | Var String
  | r :+: r
  | r :*: r
  deriving Functor
```

## The Base functor

The `Base` functor helps us establish an inital F-algebra (final F-coalgebra) for a type.

`Base` functor as an open type family:

```hs
type Base :: Type -> Type -> Type
type family Base t :: Type -> Type
```

Some instances of the `Base` type family:

```hs
type instance Base [a]     = ListF a
type instance Base (Fix f) = f
```

The `Base` type family just groups base functors under the name `Base` (we can accomplish the same also with a class-associated type).

`Base :: Type -> Type -> Type` type family takes a functor `t :: Type -> Type`, serving just as the name for a group of functors.

We can instantiate the `Base` type family with functors like `ListF a`, `[]`, `Maybe`, `Either e`, `(->) a`, etc.

One special case is instantiating it with a fixed functor `f` wrapped in `Fix f`, so that `Base (Fix f) = f`. This suggests that all fixed functors, `Fix f`, eventually reduce to `f`, which constitutes an algebra, `f a -> a`.


## Building inital F-algebra (final F-coalgebra) with Fix

```hs
newtype Fix f = Fix { unFix :: f (Fix f) }
```

`Fix (ListF a)` ≅ `ListF a (Fix (ListF a))` ≅ [a]

- `Fix (TreeF a)` Binary trees with labeled nodes
- `Fix (ExpF)`    Exp with add and mul over integers


Relation between `Fix` and `Base`:
```hs
type instance Base (Fix f) = f
```

## Generalized folds
s

```hs
class Functor (Base t) => Recursive t where
  proj :: t -> Base t t
  cata :: (Base t a -> a)    -- Base-t algebra
       -> t                  -- fixpoint
       -> a
  cata alg = alg . fmap (cata alg) . proj

instance Recursive [a] where
  proj :: [a] -> Base [a] [a]
  proj []     = NilF
  proj (x:xs) = ConsF x xs

instance Functor f => Recursive (Fix f) where
  proj :: Fix f -> Base (Fix f) (Fix f)
  proj = unFix
```
