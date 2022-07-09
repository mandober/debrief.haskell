---
url: https://cdsmithus.medium.com/fixpoints-in-haskell-294096a9fc10
article-title:    Fixpoints in Haskell
date-posted:      2018-10-15
date-downloaded:  2022-04-16
tags: fixpoint, definedness order, flat types
---
# Fixpoints in Haskell

Given an endofunction on any set, `f : A -> A`, a fixpoint of `f` is an input `x₀` that `f` maps back to `x₀`; that is, a fixpoint `x₀ ∈ A` for which 
`f(x₀) = x₀`. In the graph of the function, fixpoints are values that fall on the `x = y` line.

For example, the square function, `f(x) = x²`, has two fixpoints, 0 and 1. On the other hand, the `cos` (cosine) function has the attractor fixpoint when its input is given in radians. Namely, starting with any value, repeatedly applying `cos`, each time produces a result that gets closer to, until it finally converges to a number (~0.739). Once this fixpoint is reached, the results don't change anymore, remaining fixed at that value. This is the only fixpoint of the cosine function. For a function `id`, every argument is a fixpoint. And some functions don't have a fixpoint.


## Haskell's fix combinator

In the base package, there's a function `Data.Function.fix` defined as

```hs
fix¹ :: forall a. (a -> a) -> a
fix¹ f = let x = f x in x

fix² :: forall a. (a -> a) -> a
fix² f = x where x = f x

fix³ :: forall a. (a -> a) -> a
fix³ f = f (fix³ f)
```

The `fix` (usually defined as `fix¹` although all 3 definitions are equivalent) takes a function as input and returns the least fixpoint of that function. And it does this seemingly magically, without any further type restrictions and a very short implementation. Things must not be what they seem…

The trick is that Haskell functions aren't just ordinary functions on ordinary sets. Haskell functions can throw exceptions, and they can loop indefinitely. To reconcile this with the world of mathematical functions, we say that Haskell functions work not just with the set of ordinary values of its types, but the set of all *partially defined values*.

In math, the set of integers has just numbers, but in Haskell, the type `Integer` includes one more value, `⟘`, used to indicate that the function on the Integer set (type) may diverge. In essence, `⟘` means "I don't know".

Once `⟘` is taken into account, the function `g(x) = x + 1` that normally has no fixpoints, actually has the fixpoint now because `g(⟘) = ⟘`. That is, if you don't know what number goes in, you don't know what number comes out. And this is, in fact, the fixpoint you get from the `fix`.

The expression `fix g` loops forever instead of producing a value, so we say its value is `⟘`. In fact, any time `f(⟘) = ⟘`, `fix f` will produce `⟘`.

However, there are functions for which `⟘` is not a fixpoint because of the laziness. For example, a constant function, `h(x) = 42`, produces an output of 42 without even looking at the input when `h` is passed to `fix`. In other words, `h(y) = 42` even if `y = ⟘`. So `⟘` is not a fixpoint of `h` since the `fix h` returns 42.

## The definedness ordering

With functions over simple types like `Double`, there are only two outcomes: if the function is constant, `fix` produces the constant value; otherwise `⟘`. Types like these are called *flat* - either you know nothing about the value, or you know everything.

Types with more structure have more interesting result. A list has many levels of "unknowness", depending on which parts of the value are unknown:
- `⟘` means that nothing about the list is known
- `⟘ : ⟘` means that the list is known to be non-empty, but neither the first element, nor the rest of the list are known. This is more information than you have about `⟘` (which might be an empty list)
- `3 : ⟘` means that the list starts with a 3, but you don't know what (if anything) comes after that. This is strictly more information than `⟘ : ⟘`
- `⟘ : []` or just `[⟘]`, means that the list is known to only contain one element, but it's not known what that element is. This is again strictly more information than `⟘ : ⟘`, but it's incomparable to `3 : ⟘`. They are both more informative than just whether the list is empty, but neither is strictly more informative than the other. They provide different information.
- `3 : []` or just `[3]`, is strictly more information than either of `⟘ : []` or `3 : ⟘`

The definedness relation forms a partial order:

```
    3 : []
     / \
    /   \
   /     \
⟘ : []   3 : ⟘
   \     /
    \   /
     \ /
    ⟘ : ⟘
      |
      |
      ⟘
```

- `⟘` ⊑ `⟘ : ⟘` ⊑ `3 : ⟘` ⊑ `3 : []`
- `⟘` ⊑ `⟘ : ⟘` ⊑ `⟘ : []` ⊑ `3 : []`

The definedness order sorts the values by how much we know about them. In this order, 3 and 4 are not comparable (they are both completely defined). But `⟘` is less than `⟘ : ⟘`, which, in turn, is less than either `3 : ⟘` or `⟘ : []`, and each of these is in turn less than `3 : []`.

Any Haskell type has a complete semilattice of values in this definedness order, which means that given any nonempty collection of values, there is always some unique greatest lower bound which is the most-defined possible value that's still less defined - but compatible - with all of them.

For example, for lists of numbers, the greatest lower bound of the set `{[3, 2], [3, 1, 5], [3, 4, 5, ⟘]}` is `3 : ⟘ : ⟘`, since all three lists are compatible with this type, but making it any more defined would have to exclude some of them.

An important attribute of Haskell functions is that they preserve this definedness order - they are monotonic. In other words, if `x` is less or equally defined to `y` in this order - meaning that `x` is compatible with `y`, but possibly less defined, then `f(x)` is also less then or equal to `f(y)`.

This makes sense - if knowing a little bit about the input gives you some knowledge about the output, then learning more about the input might tell you more about the output, but it should never contradict or take away from what you already knew.

The fix combinator always produces the least fixpoint in this definedness ordering. This least fixpoint will be guaranteed to exist by the *Knaster-Tarski theorem*, which says that any order-preserving function on a complete semilattice must also have a complete semilattice of fixpoints - and in particular, there must be a least one of them.

Definedness is a complete semilattice, and all Haskell functions are order-preserving, and that's good enough to guarantee that the least fixpoint exists.

Another example: define `threeAnd list = 3 : list`. A fixpoint here is a list that is not changed by prepending a 3 onto it. It can't be `⟘`, because `3 : ⟘` is not the same as `⟘`. The answer is an infinite list of 3's, so `fix threeAnd` gives you exactly that - an infinite list of 3's. We can check this with GHCi.

## Fixpoints and recursion

The reason that fixpoints play such a dominant role in functional programming is that they are intricately related to recursion, and that relationship is an important bridge between the operational realm (understanding what the program does) and the denotational realm (understanding what the program means).

In the operational sense, Haskell's fixpoint can be defined using recursion:

```hs
fix f = x where x = f x
```

This definition seems almost too cute to work, but it does! It just starts with an `x` and then keeps replacing every `x` with `f(x)`. After an infinite number of substitutions, we find that the least fixpoint of `f` is `f(f(f(f(f(f(f(f(…))))))))`. In other words, *any time we need to refer to the input of the function, we just substitute another function application instead*.

In the opposite direction, though, general recursion can be defined in terms of the fixpoint combinator. Suppose you have a programming language with no direct recursion allowed, but you are given a fixpoint combinator. Then there's a simple syntactic sugar for recovering general recursion. Just take your recursive definition, like `x = … x …`, and rewrite it with an extra parameter that takes the place of recursive uses: `x1 x0 = … x0 …`. Notice that `x1` is not defined recursively. But setting `x = fix x1` satisfies the equation given for `x`.

In the untyped lambda calculus, `fix` is known as the *Y combinator*, and it's a crucial step to showing that the untyped lambda calculus is computationally universal as it can simulate general recursion.

However, in a typed lambda calculus it is impossible to define such a Y combinator because it would allow for nontermination (which typed lambda calculi usually try to avoid). When these calculi are used as the basis for general programming languages, the first step is usually to introduce some kind of general recursion (such as a recursive `let`) to allow for nonterminating programs to be written.

There's still an important place for a fixpoint combinator, though, If we care only about what our programs do, then adding recursion might be sufficient. But recursion literally means defining things in terms of themselves, and that's not safest thing to do if you would like words to keep having meanings that make sense. So what does a recursive definition even mean?

The Knaster-Tarski theorem guarantees that any function has a unique least fixpoint in the definedness order. So, at least for the pure fragment of Haskell, it's okay to just define the meaning of a recursive definition to be the least fixed point of the related non-recursive function obtained by pulling recursive uses into a parameter. This ensures that any recursive definition can be given a solid meaning, even though on face value it's just a circular definition.

(Incidentally, fixpoints are a nice way out of all kinds of self-referential paradoxes, what Hofstadter called "strange loops". Remember, there's no paradox in time travel as long as the universe is a fixpoint of the function obtained by pulling the references out as parameters.)

Looping back to the example of calculating square roots by iterative refinement approximation, we could redefine the fixpoint function using iterative refinement to close in on the right value of the square root by using the least fixpoint of a Haskell function.

## Newton's method

The MIT book "Structure and Interpretation of Computer Programs" deals with progressively more abstract representations of algorithms for computing a square root.

Heron of Alexandria proposed an algorithm to calculate the square root of a natural number `n`. Also known as Newton's method and Newton-Raphson method.



Q: Calculate the square root of `n` as a number `x` s.t. `√n ≅ x`

1. Start with a guess, `x₀`

2. Compare the guess `x` with `n / x`. Since`x² = n`, `x = n / x`, 
  so `x` and `n / x` should be equal when `x = √n` or `x² = n`.

  Check if `x` is a good enough solution.
  A "good enough" result could be a number `m = a²` 
  such that `|n - m| < ϵ` where `ϵ = 0.0001` or similar.

  If `m ≅ ϵ`
  then return `a`
  else goto (4)

4. Average `x` and `n/x`

       xₙ + (n / xₙ)
  xₙ﹢₁= ------------      `x₁` = (x₀ + n / x₀) / 2`
           2

  goto 1 with the average as the new guess.


[Calculate] `x` for a given number `n` such that `√n ≅ x`

```js
x = √n
x² = n
x = n / x

x² = n
x∙x = n
x₁∙x₂ = n
x₁ = n / x₂
x₂ = n / x₁
```

The equation `x = n / x` presents the idea that `x` (being the square root of `n`) should be equal to `n / x`. However, it doesn't really hurt it they are not exactly equal - it would be acceptable if, e.g. their difference is less than some small number (epsilon).

`x - (n / x) < ϵ` where `ϵ = 1e-6`



1. initial guess `a` (that `a² = n` i.e. that a = n² i.e. √n = a)


For example, if we start by guessing that the square root of 10 is 5, then we go through these steps:
a = 5, but n / a = 2. Those are not close, so our new guess is 7/2.
a = 7/2 (3.5), but n / a = 20/7 (about 2.86). Those are not close, so our new guess is 89/14.
a = 89/28 (about 3.18), but n / a = 280/89 (about 3.15). Those are not quite close enough, so our new guess is 15761/4984.
a = 15761/4984 (about 3.16), and n / a = 49840/15761 (also about 3.16). Those pretty close, so we can stop.
