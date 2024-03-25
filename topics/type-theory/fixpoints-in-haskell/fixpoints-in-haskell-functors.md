---
url: https://free.cofree.io/2019/08/21/mu-nu/
article-title:    Fixed Points and Non-Fixed Points of Haskell Functors
date-posted:      2019-08-21
date-downloaded:  2022-04-18
tags: fixpoint, definedness order, flat types
---
# Fixpoints in Haskell

I've been playing with fixed points of Haskell functors lately and thought it would be useful to write down some of my thoughts, both to solidify my understanding and for future reference. If it happens to be helpful for someone else, then all the better. I guess I'll start with Haskell's category.

## Haskell Category

The underlying category of Haskell is called `Hask`. While sometimes we can simply think of it as `Set`, the category of sets and total functions, `Set` is, in many cases, an oversimplified approximation of Hask. A much better approximation is a category called `CPO⊥` (also known as `SCPO` or `CPPO⊥`).

CPO⊥ is a category where

* Objects are pointed cpos. 
  A pointed cpo is 
  a pointed complete partial order `(A, ≼)` 
  such that it is

  1. *pointed*: 
    there exists a bottom element `⊥ ∈ A` 
    such that `∀a ∈ A. ⊥ ≼ a`

  2. *ω-complete*: 
    every ascending chain `<a> = a0 ≼ a1 ≼ a2 ≼ …` 
    has a least upper bound `⊔<a> ∈ A`. 
    A least upper bound (lub) of `<a>` 
    is an element `a` such that 
    `∀i. aᵢ ≼ a`, 
    and `∀a' ∀i. aᵢ ≼ a' -> a ≼ a'`

* Arrows are strict continuous functions

  1. a *strict function* `f` 
    is a function that preserves bottom, `f ⊥ = ⊥`

  2. a *continuous function* `f` 
    between cpos `(A, ≼)` and `(B, ⋞)` 
    is a function that is 
    monotonic, `a ≼ b -> f a ⋞ f b`, and 
    preserves least upper bounds of ascending chains, 
    `f (⊔<a>) = ⊔(<f a>)`


Let's look more closely at the objects and the arrows in CPO⊥ and their relationships with Haskell types and functions


## Objects in CPO⊥

Pointed cpos can model Haskell types and functions.

The partial order `≼` is an order with respect to *degree of definedness* or approximation.
- `⟘` is the least defined value
- `x ≼ y` if `x` is less defined than `y` (i.e. if `x` approximates `y`)
- fully defined values are not comparable with one another in this order

For example, for the natural number data type:

```hs
data Nat = Zero | Succ Nat
```

we have
- `⟘ ≼ x` for all `x :: Nat`
- `Succ ⟘` ≼ `Succ (Succ ⟘)` ≼ `Succ (Succ Zero)`
- `Succ (Succ ⟘)` ⋠ `Succ Zero`
- `Succ Zero` ⋠ `Succ (Succ Zero)`

The partial order of the values of `Nat` can be pictured as

```js
                    ∞
                   .
                  .
   S (S Z)       .
         \      /
          \    /
           \  /
   S Z     S (S ⟘)
     \      /
      \    /
       \  /
Z      S ⟘
 \      /
  \    /
   \  /
    ⟘
```

* `x ≼ y` iff there is a path from `x` to `y` in the diagram going upwards.
* `∞` is the value `Succ (Succ (Succ ...))` with infinitely many successors, or equivalently

```hs
let inf = Succ inf in inf
```

* `∞` must exist because `⟘, Succ ⟘, Succ (Succ ⟘), …` is an ascending chain which must have a lub.

For comparison, if we make `Succ` ctor strict, such that `Succ ⟘ = ⟘`, then the cpo becomes flat (except for `⟘`):

```js
Z    S Z   S (S Z)   …
.     .    .
 .   .  .
  . . .
    ⟘
```

and there is no `∞` value.


For function types like `A -> B`, the order `≼` is defined as    
`f ≼ g ⇔ ∀x. f x ≼ g x`

`f` approximates `g` iff every value of `f` approximates the corresponding value of `g`.

## Arrows in CPO⊥

### Continuous Functions

Continuous functions can be used to model computable/implementable functions.

As a counter example, the function `f` defined as

```hs
f :: Int -> Int
f x = if x is ⟘ then 0 else x
```

is not monotonic, since `⟘ ≼ 1` but `f ⟘ = 0` ⋠ `1 = f 1`.

Indeed, `f` is not implementable in Haskell. The only implementable function that maps `⟘` to `0` is `const 0`, which maps everything to 0.

As another counter example, consider the function `g` defined as

```hs
g :: (Int -> Int) -> Int
g f = if f is a total function then 0 else ⟘
```

`g` is monotonic but not continuous because it doesn't preserve least upper bounds of ascending chains.

To see why, consider an ascending chain `<f> = f0 ≼ f1 ≼ f2 ≼ …` consisting of functions of type `Int -> Int`. And functions `fᵢ` are defined by

```hs
fᵢ :: Int -> Int
fᵢ x = if x <= i then x else ⟘
```

Since `fᵢ` is not a total function for any `i`, we have `g fᵢ = ⟘` for all `i`. Thus `⊔(<g f>) = ⟘`.

On the other hand, `⊔<f>` is the identity function, which is total, so   
`g (⊔<f>) = 0`

Therefore, `g` is not continuous. And it is indeed not implementable in Haskell, because to tell whether an arbitrary function is total, one would need to solve the halting problem.

### Why Strict

It seems odd that we are modeling Haskell functions with a category CPO⊥ that only has strict functions.

> A non-strict function can be modeled by a strict function.

Are strict continuous functions sufficient to model all Haskell functions, including non-strict ones?

The answer is yes: a non-strict function `f :: A -> B` can be modeled by a strict function of type `f' :: Maybe A -> B` defined as

```hs
f' ⟘ = ⟘          -- f' is strict
f' Nothing  = f ⟘
f' (Just a) = f a
```

`f'` is a strict function that is equivalent to `f`.

A number of useful theorems are only applicable to CPO⊥ (and not applicable to CPO, where functions are allowed to be non-strict), such as the existence of initial algebras for locally continuous functors.

## Hask is Not CPO or CPO⊥

There are some important differences between Hask and CPO/CPO⊥.

Hask is not even a category because the existence of `seq` means    
`undefined . id ≠ undefined`.

The wiki page proposes a solution, which is to define arrow identities extensionally (`f` and `g` are considered the same arrow if `∀x. f x = g x`), but as Andrej Bauer pointed out, it is not a satisfying solution since `f x = g x` is not well defined.

For now, let's ignore `seq` and assume that Hask is indeed a category.

Both CPO and CPO⊥ have categorical products, but Hask does not.

In CPO/CPO⊥, the product of two pointed cpos is simply their cartesian product, with `(⊥, ⊥)` being the bottom element. 

In Hask, however, the lazy pair, `(,)`, is not a categorical product. This is because `(,)` is a lifted cartesian product, i.e. `⊥ :: (a, b)` and `(⊥, ⊥) :: (a, b)` are two distinct values, and both `x = ⊥` and `x = (⊥, ⊥)` satisfy

```hs
fst x = ⊥
snd x = ⊥
```

so we lose the uniqueness of the mediating arrow in the universal property of categorical products.


The strict pair:

```hs
data P a b = P { fstS :: !a, sndS :: !b }
```

is not a categorical product either, because it is too strict: 
not only `P ⊥ ⊥ = ⊥`, but also `P x ⊥ = P ⊥ y = ⊥` for all `x` and `y`.

In order for it to be a categorical product, the universal property mandates that `fstS (P x y) = x` for all `x` and `y`, but `fstS (P x ⊥) = ⊥`.

Similarly, CPO⊥ has coproducts (although CPO does not), but in Hask, neither `Either a b` nor its strict counterpart is a categorical coproduct.

To sum up, *Hask* category is messy. It would be much better if it was as nice as `Set` or even `CPO`/`CPO⊥`, and we indeed sometimes reason about Haskell programs as if we have Set or CPO/CPO⊥, but it's good to be aware of the subtle differences.

## Fixed Points of Endofunctors in Hask

Now let's look at the properties of fixed points for *Hask* functors, and what are and aren't fixed points a Haskell functor.

### TL;DR

* In Haskell, the least fixed point and greatest fixed point of a functor always coincide (however, see the next bullet point).

* The universal property of initial algebras in Hask, like CPO, has an additional strictness requirement. Strictly speaking, all functors in Hask do not have least fixed points or initial algebras. But we usually implicitly assume the strictness condition, and regard Hask functors as having least fixed points and initial algebras indeed.

* Just because the least fixed point and the greatest fixed point of a functor coincide does not mean the functor has a unique fixed point (up to isomorphism).

* `Mu F` is both the least and the greatest fixed point of any Haskell functor `F`. If `F` is lazy, this is also the case with `Nu F`. If `F` is strict, however, `Nu F` is not even a fixed point (`Mu` and `Nu` are defined in `Data.Functor.Foldable`).

## Fixed Points of NatF

As an example, consider one of the simplest functors, the base functor for natural numbers:

```hs
data NatF a = ZeroF | SuccF a
```

The lazy natural number type `Nat` is a fixed point of `NatF`.

```hs
data Nat = Zero | Succ Nat
```

The isomorphism between `NatF Nat` and `Nat` is witnessed by the `embed` and `project` functions:

```hs
embed :: NatF Nat -> Nat
embed ZeroF     = Zero
embed (SuccF n) = Succ n

project :: Nat -> NatF Nat
project Zero     = ZeroF
project (Succ n) = SuccF n
```

It is easy to verify that `embed . project = id` and `project . embed = id`.



(...)
