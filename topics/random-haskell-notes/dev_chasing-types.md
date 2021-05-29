# Chasing types in Haskell

<!-- TOC -->

- [Introduction](#introduction)
- [The setup](#the-setup)
- [Logical derivation](#logical-derivation)
- [Back to the function implementation](#back-to-the-function-implementation)
- [Mapping continuations](#mapping-continuations)
  - [First version](#first-version)
  - [Second version](#second-version)
- [Chaining continuations](#chaining-continuations)

<!-- /TOC -->

## Introduction

*Type chasing* is a situation you find yourself in when trying to come up with a solution to a, usually polymorphic, function's definition in Haskell. Having worked out a function's signature, all that remains is to write the function's implementation driven by those types. But instead, you're driven to tears.

## The setup

This is the exact content of my editor that has frustrated me for too long.

```hs
kapp :: (((a -> b) -> r) -> r)    -- ContT r m (a -> b)
     -> ((a -> r) -> r)           -- ContT r m a
     -> ((b -> r) -> r)           -- ContT r m b
```

I was trying to implement this, arbitrarily named, `kapp` function above, which is actually based on the `ContT`'s instance for Applicative class' `<*>` method, found in the `transformers` package here: https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Cont.html#line-168

```hs
-- (from transformers package)

-- ContT as a monad transformer
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- ConT's Applicative instance
instance Applicative (ContT r m) where
  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)
```

Apart from the type parameter `m`, the two implementation should be fairly the same, save for ContT's re/un/wrapping. Staring some more at `kmap` but failing to find a way forward, I've switched to staring at the solution (`<*>` above). It was particularly frustrating how they knew to introduce that lambda there but apply it way over there. And the other lambda as well. It was exactly these lambda introductions that reminded me of the inference rules in propositional logic, specifically implication introduction, so I've decided to Curry-Howard the shit out of this thing.

## Logical derivation

The inference rule of implication introduction states that if you assume a `p` and work your way to a `q`, then you can conclude `p => q`. Instead of coming up with an example for implication introduction, we better return to the problem at hand and demonstrate it there.

```hs
kapp :: (((a -> b) -> r) -> r)    -- h
     -> ((a -> r) -> r)           -- k
     -> (b -> r)                  -- g
     -> r
kapp h k g = -- how now brown cow?
```

Summary:
* we have 3 function arguments bound to parameters `h`, `k`, `g`
  - `h :: ((a -> b) -> r) -> r`
  - `k :: (a -> r) -> r`
  - `g :: b -> r`
* we need to produce:
  - `r` type

The 3 arguments are the 3 given propositions, and we need to come up with the conclusion `r`. The form of this function is appropriate for conversion to the Fitch-style proof derivation because everything needed to solve it is already present, i.e. (peeking at the solution) we know that no arbitrary (Prelude) function is involved, everything needed for a solution is available in the form of identifiers, we just need to rearrange them somehow. And since the types involved are function types, they translate directly into implications.

The Fitch-style diagram:

```
┌──┬──────────────────────┬────────────────────────┐
│1 │ ((a -> b) -> r) -> r │ proposition            │
│2 │ (a -> r) -> r        │ proposition            │
│3 │ b -> r               │ proposition            │
╞══╪══════════════════════╪════════════════════════╡
│4 │ ┌ (a -> b)¹          │ assumption¹            │
│5 │ │ ┌ a²               │ assumption²            │
│6 │ │ │ b                │ MP 4,5                 │
│7 │ │ │ r                │ MP 3,6                 │
│8 │ │ a² -> r            │ ->I 5-7 (discharging²) │
│9 │ │ r                  │ MP 2,8                 │
│10│ (a -> b)¹ -> r       │ ->I 4-9 (discharging¹) │
│11│ r                    │ MP 1,10                │
└──┴──────────────────────┴────────────────────────┘
```

We need to come up with the conclusion `r`. Lines 1-3 are given. So we start the solution on line 4.
4. We assume `(a -> b)`
5. We make another assumption `a`
6. Modus ponens: if we have `(a -> b)` (4) and an `a` (5), we can conclude `b`
7. Modus ponens: `(b -> r)` (3) and `b` (6), we conclude `r`
8. Discharging assumption 2, we obtain `(a -> r)`
9. Modus ponens: `(a -> r) -> r` (2) and `(a -> r)` (8), produce `r`
10. Discharging assumption 1, obtain `((a -> b) -> r)`
11. Modus ponens: `((a -> b) -> r) -> r` (1) and above (10) give `r`

And that `r` is the conclusion we sought. QED.

I was hoping this derivation form would reveal more so that the implementation of a function's definition would be compressed into a series of forced steps. But anyway, it seems as a helpful aid when you're stuck with an equation.

When doing a derivation, you can assume anything. Each assumption gets indented a level to the right as a reminder that you can only use the given propositions and the formulas from the same level as that assumption; also, it reminds that an assumption would have to be discharged.

When discharging a particular assumption, the form that is written on a line is always the same: first write that assumption, followed by an implication sign, followed by whatever formula was on the previous line (the line directly above the implication introduction). The justification given is called the *implication introduction*, abbreviated by `->I`, along with the range indicating its scope.

*Implication elimination*, or `->E` or `MP` (modus ponens), is equivalent to function application: having an implication `a -> b` and an `a`, you can conclude a `b`.

## Back to the function implementation

```hs
kapp :: (((a -> b) -> r) -> r)    -- h
     -> ((a -> r) -> r)           -- k
     -> (b -> r)                  -- g
     -> r
kapp h k g = h (\f -> k (\x -> g (f x)))
-- i.e.
kapp h k g = h \f -> k \x -> g $ f x
kapp h k g = h \f -> k (g . f)
```

The derivation with code reference :

```
4 │ ┌(a -> b)¹     assumption¹            . a -> b      :: f
5 │ │ ┌a²          assumption²            . . a         :: x
6 │ │ │b           MP 4,5                 . . b         :: f x
7 │ │ │r           MP 3,6                 . . r         :: g (f x)
8 │ │ a² -> r      ->I 5-7 (discharging²) . a -> r      :: c
9 │ │ r            MP 2,8                 . r           :: k c
10│(a -> b)¹ -> r  ->I 4-9 (discharging¹) (a -> b) -> r :: d
11│r               MP 1,10                r             :: h d
```

It may not be crystal clear at first, but after some exercises, the steps behind many function implementations become easier to justify.

---

(hmm, what if the first assumption was `a`?)

```hs
kapp :: (((a -> b) -> r) -> r)
     -> ((a -> r) -> r)
     -> (b -> r)
     -> r

kapp h k g = h $ \f -> k $ \a -> g $ f a
-- vs
kapp h k g = k $ \a -> h $ \f -> g $ f a
```

Same signature, different derivation

```
┌──┬──────────────────────┬────────────────────────┐
│1 │ ((a -> b) -> r) -> r │ proposition            │
│2 │ (a -> r) -> r        │ proposition            │
│3 │ b -> r               │ proposition            │
╞══╪══════════════════════╪════════════════════════╡
│4 │ ┌ a¹                 │ assumption¹            │
│5 │ │ ┌ (a -> b)²        │ assumption²            │
│6 │ │ │ b                │ MP 5,4                 │
│7 │ │ │ r                │ MP 3,6                 │
│8 │ │ (a -> b)² -> r     │ ->I 5-7 (discharging²) │
│9 │ │ r                  │ MP 1,8                 │
│10│ a¹ -> r              │ ->I 4-9 (discharging¹) │
│11│ r                    │ MP 2,10                │
└──┴──────────────────────┴────────────────────────┘

kapp h k g = k $ \a -> h $ \f -> g $ f a
```

Unlike the first one, this derivation matches the implementation in code, since `a` is assumed first, while `a -> b` is assumed second, both, in the diagram and in the code... but something's gotta be wrong somewhere (!?)

## Mapping continuations

Leaving the two ambiguous `kapp` implementations above for now (lest interfere with the ongoing investigation), let's see the thing that should have been presented first - mapping a continuation by implementing the standalone function similar to `fmap` but renamed to `kmap`.

The signature of `kmap` is more easily understood: the callback passed to a function, `k`, is passed a function's would-be-returned value, `a`, along with a mapping function `f` that will be applied to `a` before passing it on to `k`. So, a function's original return value is caught right before being passed to the continuation and mapped (and then passed to `k`).

### First version

Like before, the signature below can be simplified by removing the extra parenthesis on the right, so we get 3 proper arguments. It is actually equivalent whether we declare 3 parameters on the LHS of an equation, or we catch each argument with a lambda on the RHS, or any distribution in between. But for the first version of `kmap`, all 3 arguments will have a corresponding proper, i.e. LHS, declared parameter.

```hs
-- v.1
kmap :: (a -> b) -> ((a -> r) -> r) -> (b -> r) -> r
-- just to make it more readable
kmap :: (a -> b)            -- f
     -> ((a -> r) -> r)     -- k
     -> (b -> r)            -- g
     -> r
kmap f k g = k (\a -> g (f a))
```

The implementation now follows smoothly from the corresponding Fitch diagram:

```
a -> b, (a -> r) -> r, b -> r ⊢ r

1 │ a -> b          proposition            | f
2 │ (a -> r) -> r   proposition            | k
3 │ b -> r          proposition            | g
--|----------------------------------------|------------------
4 │ ┌ a¹            assumption¹            | \a -> (
5 │ │ b             MP 1,4                 |   f a
6 │ │ r             MP 3,5                 |   g (f a)
7 │ (a¹ -> r)       =>I 4-6 (discharging¹) | )
8 │ r               MP 2,7                 | k $ \a -> g (f a)
```

### Second version

Another version of `kmap`, but this time the third argument doesn't have a corresponding parameter on the LHS, rather it is bound by introducing an extra lambda. This is often the case when dealing with the continuations (and other types as well) wrapped inside a newtype, because then the data constructor gets in the way, forcing the introduction of a lambda to handle it (usually the last, often the third, argument). Or maybe because you'd just prefer to infix the function you're defining, in which case 2 parameters on the LHS are just right (e.g. `f 'kmap' k = \g -> …` instead of `kmap f k g = …`).

Now we have 2 premises instead of 3, and the consequent we're looking for is not just `r` but `(b -> r) -> r`. However, the entire effect of that is the introduction of another assumption, which, translated to the code, means introduction of an extra lambda (whose body will wrap the implementation from the first version), as shown below.

```
a -> b, (a -> r) -> r ⊢ (b -> r) -> r

1 │ a -> b          proposition            | f
2 │ (a -> r) -> r   proposition            | k
--|----------------------------------------|--------------------
3 │ ┌ (b -> r)¹     assumption¹            | \g -> (
4 │ │ ┌ a²          assumption²            |   \a -> (
5 │ │ │ b           MP 1,4                 |        f a
6 │ │ │ r           MP 3,5                 |     g (f a)
7 │ │ (a² -> r)     =>I 4-6 (discharging²) |   )
8 │ │ r             MP 2,7                 |   k $ \a -> g (f a)
9 | (b -> r)¹ -> r  =>I 3-8 (discharging¹) | )
```

The diagram now translates to the following implementation:

```hs
-- v.2
kmap :: (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r)
-- making the dig more readable
kmap :: (a -> b)            -- f
     -> ((a -> r) -> r)     -- k
     -> ((b -> r) -> r)
kmap f k = \g -> k (\a -> g (f a))
```

## Chaining continuations

To complete the trio, the definition of the `bind`-like function for chaining continuations follows.

```hs
kbind :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
-- rearranged sig
kbind :: ((a -> r) -> r)
      -> (a -> ((b -> r) -> r))
      -> ((b -> r) -> r)

-- removing redundant parens
kbind :: ((a -> r) -> r)        -- k
      -> (a -> (b -> r) -> r)   -- h
      -> b -> r                 -- g
      -> r
kbind k h g = k \a -> (h a g)
```

This implementation follows from the diagram smoothly:

```
(a -> r) -> r, a -> (b -> r) -> r, b -> r ⊢ r

1 | (a -> r) -> r        proposition    | k
2 | a -> (b -> r) -> r   proposition    | h
3 | b -> r               proposition    | g
--|-------------------------------------|-----------------
4 | ┌ a¹                 assumption¹    | \a -> (
5 | │ (b -> r) -> r      MP 2,4         |    h a
6 | │ r                  MP 5,3         |   (h a) g
7 | a¹ -> r              =>I 4-6 (dis¹) | )
8 | r                    MP 1,7         | k (\a -> h a g)
```
