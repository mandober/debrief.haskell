# Definedness

https://cdsmithus.medium.com/fixpoints-in-haskell-294096a9fc10

The `fix` function from `Data.Function` can compute a fixpoint of any Haskell function because Haskell functions aren't total mathematical functions defined on the entire domain set after all: they can diverge (error out, loop forever, etc.).

```hs
fix :: (a -> a) -> a
fix f = f (fix f)
```

To reconcile this with the world of mathematical functions, we say that Haskell functions work not just with the set of ordinary values of its types, but the set of all partially defined values. Unlike ℤ, Haskell's types `Integer` or `Int` are *lifted*: they include one more value, denoted `⊥` (bottom). Bottom indicates that a function may diverge instead of producing a proper value. Essentially, `⊥` represents the unknown.

Once `⊥` is taken into account, all functions actually have a fixpoint, since `f ⊥ = ⊥`. In fact, this is the fixpoint the `fix` function produces. The expression `fix g`, for some function `g`, diverges, so we say it evaluates to `⊥`. In fact, any time `f ⊥ = ⊥` then `fix f` will produce `⊥`. But due to laziness there are functions whose fixpoint is not ⊥, like constant functions, like `h x = 42`. Haskell produces 42 without even looking at the input. In other words, `h ⊥ = 42`. That is, ⊥ is not a fixpoint of this function, in fact, `fix h = 42`.




## The definedness ordering

With functions on flat types (Int, Double, etc.), there are the only 2 possibilities: with constant functions, `fix` produces the constant value, otherwise `⊥`. These type are called `flat` because you either you know nothing about a value of this type, or you know everything.

However, with types that have more structure, the findings are more interesting. A list has many forms where different parts of it are unknown:
- `⊥` means nothing is known about it
- `⊥ : ⊥` means the list is known to be non-empty, but neither the head nor the tail is known. This is more information than above which might be an empty list
- `3 : ⊥` means the head is 3, but you don't know what, if anything, comes after that. This is strictly more information than `⊥ : ⊥`
- `⊥ : []`, or just `[⊥]`, means that the list is known to only contain one element, but it's not known what that element is. This is again strictly more information than `⊥ : ⊥`, but it's incomparable to `3 : ⊥`. They are both more info than just whether the list is empty, but neither is strictly more informative than the other. They provide different information.
- `3 : []`, or just `[3]`, is strictly more information than either of `⊥ : []` or `3 : ⊥`

This defines a *partial order* on Haskell values, which sorts the values by definedness (how much info is known). In this partial order, neither `3` or `4` is less-than the other since they are both completely defined. But `⊥` is less than `⊥ : ⊥`, which is in turn less than either `3 : ⊥` or `⊥ : []`, and each of these is in turn less than `3 : []`.

In fact, any Haskell type has a **complete semilattice** of values in this definedness order, which just means that given any nonempty collection of values, there is always some unique *greatest lower bound*, which is the most-defined possible value that's still less defined - but compatible - with all of them.

For example, for lists of numbers, the greatest lower bound of the set S =      `{[3, 2], [3, 1, 5], [3, 4, 5, ⊥]}` is `3 : ⊥ : ⊥`, since all 3 lists are compatible with this type, but making it any more defined would have to exclude some of them.

> Haskell functions preserve the definedness order.

If `x ⊑ y` (meaning that `x` is compatible with `y` but possibly less defined than it) then `f(x) ⊑ f(y)`.

This makes sense: if knowing a little about the input gives you some knowledge about the output, then learning more about the input might tell you more about the output, but it should never contradict or take away from what you already knew.

> *Knaster-Tarski fixpoint theorem*: any order-preserving function on a complete semilattice must also have a complete semilattice of fixpoints, and in particular, there must be a least one of them.

Now here's where everything starts to come together. The `fix` combinator always produces the least fixpoint in this definedness ordering. The least fixpoint is guaranteed to exist by the Knaster-Tarski theorem. *Definedness is a complete semilattice*, and all Haskell functions are definedness ordering-preserving, and that's enough to guarantee that the least fixpoint exists.


Let's look at another example: define `h xs = 3 : xs`. **A fixpoint of `h` is a list that is not changed at all by prepending a 3 onto it**. It can't be `⊥`, because `3 : ⊥` is definitely not the same as `⊥`. The answer is an infinite list of 3's, so `fix h` gives you exactly that.

```hs
import Data.Function (fix)
-- is defined as:
fix :: (a -> a) -> a
fix f = f (fix f)

h :: [Int] -> [Int]
h xs = 3 : xs

fix h -- 3:3:3:3:3...
```

## Fixpoints and recursion

The reason that fixpoints play such a dominant role in FP is their relation to recursion, and that relation is an important bridge between the operational realm, understanding what the program does, and the denotational realm, understanding what the program means.

In the operational sense, the `fix` can be defined using recursion:

```hs
fix f = x where x = f x
```

This definition works by starting with an `x`, and then keeps replacing every `x` with `f x`. After an infinite number of substitutions, we find that the least fixpoint of `f` is `f (f … (f (…))…)`. Any time we need to refer to the input of the function, we just substitute another function application instead.

In the opposite direction, *general recursion* can be defined in terms of the fixpoint combinator. If a language lacks support for direct recursion, but you are given a fixpoint combinator, then there's a simple syntactic sugar for recovering general recursion. Take your recursive definition `x = … x …`, and rewrite it with an extra parameter that takes the place of recursive uses:     `x₁ x₀ = … x₀ …`. Notice that `x₁` is not defined recursively. But setting `x = fix x₁` satisfies the equation given for `x`.

In the untyped lambda calculus, fix is known as the Y combinator. However, is typed lambda calculi it is not possible to define the Y combinator directly because it would allow for nontermination, which typed lambda calculi usually try to avoid. When these calculi are used as the basis for general PL, the first step is usually to introduce some kind of general recursion, such as a recursive let, to allow for nonterminating programs to be written.

Fortunately, the Knaster-Tarski theorem guarantees that any function has a unique least fixpoint in the definedness order. So, at least for the pure fragment of Haskell, it's okay to just define the meaning of a recursive definition to be the least fixed point of the related non-recursive function obtained by pull recursive uses into a parameter. This ensures that any recursive definition can be given a solid meaning, even though on face value it's just a circular definition.

  Aside: Incidentally, fixpoints are a nice way out of all kinds of self-referential paradoxes like this - what Hofstadter called *"strange loops"*. As for the time-travel and the integrity of the space-time continuum, remember that there's no paradox in time travel as long as the universe is a fixpoint of the function obtained by pulling the back references out as parameters.


We can now define the fixpoint function of Heron's square root approach: using iterative refinement to close in on the right value of the square root, by using the least fixpoint of a Haskell function.


## A puzzle

Putting together all that we've talked about, here's a question. Suppose you type the following into GHCi:

```hs
ghci> import Data.Function
ghci> fix error
```

What would happen?

To solve the puzzle, let's first explore the types: `error :: String -> a` cannot reasonably have a fixpoint unless the domain and codomain are the same, so the type specializes to `String -> String`. Then `fix error` has the type `String`.

What is this magical string that fixes all errors? Well, it must be a fixpoint of the `error` function, but the result of the `error` function is always `⊥`. And that is its whole purpose. So `fix error` must be `⊥`, too. Not so special after all?

Ah, but on the operational side, there are many kinds of `⊥`. Some are just infinite loops, others throw exceptions. But if they throw exceptions, then there's an exception value to throw. These are all invisible to the pure fragment of Haskell, but immensely important to what the program does!

To understand what's going on here, recall that `fix error = error (error (error ...)))`, an infinite nesting of `error` functions. The result is quite interesting:

```hs
ghci> fix error
-- "*** Exception: *** Exception: *** Exception: *** Exception: *** ^C
```

Let's just say it did not fix any errors. Instead, it threw an exception, with an associated exception message, that itself throws an exception when printed, and that exception has another exploding error message, and so on, until interrupted.
