# Arrows

Arrows: A General Interface to Computation: Introduction
https://www.haskell.org/arrows/index.html


## Arrow class

A computation that takes inputs of one type and produces outputs of another type, had prompted Hughes to introduce a class of binary type ctors, along with a set of axioms.

```hs
  -- Each function may be treated as a computation
arr   :: Arrow arr => (b -> c) -> arr b c

-- Computations may be composed, by connecting the
-- output of the first to the input of the second
(>>>) :: arr b c -> arr c d -> arr b d

(>>>) :: forall k (cat :: k -> k -> *) (arr :: k) (b :: k) (c :: k)
      .  Control.Category.Category cat
      => cat arr b
      -> cat b c
      -> cat arr c

(<<<) :: forall k (cat :: k -> k -> *) (b :: k) (c :: k) (arr :: k)
      .  Control.Category.Category cat
      => cat b c
      -> cat arr b
      -> cat arr c


-- A computation may be applied to a part of the
-- input with the rest passed through to the output
first  :: arr b c -> arr (b,d) (c,d)

second :: Arrow arr => arr b c -> arr (d, b) (d, c)

(&&&) :: Arrow arr => arr b c -> arr b c' -> arr b (c, c')
(***) :: Arrow arr => arr b c -> arr b' c' -> arr (b, b') (c, c')
```

## Complete Arrow class


```hs
-- in Control.Arrow
class Category arr => Arrow (arr :: * -> * -> *) where
    arr    :: (b -> c) -> arr b c
    first  :: arr b c  -> arr (b, d) (c, d)
    second :: arr b c  -> arr (d, b) (d, c)
    (***)  :: arr b c  -> arr b' c' -> arr (b, b') (c, c')
    (&&&)  :: arr b c  -> arr b  c' -> arr  b      (c, c')
{-# MINIMAL arr, (first | (***)) #-}

instance Monad m => Arrow (Kleisli m)
instance Arrow (->)
```


## The axioms

Instances of the `Arrow` class should satisfy the following laws:
* `arr id = id`
* `arr (f >>> g) = arr f >>> arr g`
* `first (arr f) = arr (first f)`
* `first (f >>> g) = first f >>> first g`
* `first f >>> arr fst = arr fst >>> f`
* `first f >>> arr (id *** g) = arr (id *** g) >>> first f`
* `first (first f) >>> arr assoc = arr assoc >>> first f`
  where                    assoc ((a,b),c) = (a,(b,c))

The other combinators have sensible default definitions, which may be overridden for efficiency.



This and related classes are in the `Control.Arrow` module (now distributed with all Haskell implementations since GHC v.?). Researches in the field of denotational semantics have independently defined equivalent structures, called *Freyd-categories*, or more generally, *pre-monoidal notions of computation*.

- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Arrow.html
- https://www.haskell.org/arrows/biblio.html#categories


## Potential Arrow types

There are many types that could be described as morphisms or some kind of transformations, in that they take an input of one type and transform it in some way, before returning, usually embellished output value of another type. Najority if such data types could be expressed as *Hughes' Arrows*, that is, they could be made instances of `Arrow` class.

Arrow-able types
* ordinary functions,        `b ->   c`
* Kleisli arrows, `Monad m => b -> m c`
* dual Kleisli arrows, `Comonad w => w b -> c`
* stream transformers, `Stream b -> Stream c`
  These may be used to embed dataflow languages [Pat01]
* simple automata: Another model of dataflow languages uses the type
  `newtype Auto b c = Auto (b -> (c, Auto b c))`
* Fudgets-style stream processors
  These are processes that can decide whether to input or output at each step:
    `data SP a b = Put b (SP a b) | Get (a -> SP a b)`
  These may be cast as arrows [Hug00], but are usually used as dual arrows
* state/behaviour transformers: `(S -> a) -> (S -> b)` for any set `S`. If S
  represents locations, the arrow describes data parallel computations [Pat01] If S represents time, the arrow describes behaviour transformers [CE01]
* hyperfunctions, with the weird datatype:
    `newtype Hyper b c = H (Hyper c b -> c)`
  can be shown to be an arrow [KLP01]
* static arrows, `F (b -> c)` where `f` is `Applicative`. Monads are more than enough, we need only `return` and `liftM2`

In many of these cases, we can generalize from (->) to any arrow (possibly constrained), yielding arrow transformers.
