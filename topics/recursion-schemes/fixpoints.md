# Fixpoints

A fixed point of a function `f` is a value `x₀` such that `f x₀ = x₀`. More generally, a fixpoint of an arrow, functor, natural transformation and similar function-like entities that can be applied to a value like functions, is any value that is returned unchanged.

A function can have none, one, many or even an infinite number of fixpoints. For example, the successor function has none, the identity function has infinitely many, the Fibonacci function has 3 fixpoints (0, 1, 5).

Usually, the discussion of fixpoints includes number-valued functions since these are the most common in math. However, things get more interesting with higher-order functions that return functions-as-values.

In lambda calculus all values are actually functions - there is nothing but functions, so it is interesting to see if such functions have a fixpoint. The fact that they do has long been discovered by Haskell Curry and his paradoxical Y combinator, although other fixpoint combinators exists as well.

The search for a fixpoint combinator (a combinator is a closed function having no free variables) was prompted by the need to express recursion in a restricted environment that is untyped lambda calculus. A piece of the puzzle was provided by the *ω combinator*, `λf.ff`, which encapsulates the notion of self-application. The ω takes an argument (which in LC is always a function) and applies that argument to itself.

When applying `ω` to functions like `I` (identity), the reduction terminated fine: `ωI = (λf.ff)(λx.x) = (λx.x)(λx.x) = λx.x`. But the application of ω to itself caused the reduction to diverge: `ωω = (λf.ff)(λf.ff) = (λf.ff)(λf.ff) = (λf.ff)(λf.ff) = …`. The application `ωω` was named the *Ω combinator* (big omega), being based on little omega. The Ω was the way to accomplish unbounded recursion, so the research has then focused on figuring out a way toward the bounded, controlled recursion.

As mentioned, a fixpoint (or fixed point) of a function `f` if a value `x` such that `f x = x`; only in LC, the argument `x` is actually a function arg (farg).

- `f` is a function (lambda abstraction) whose fixpoint we want to find
- a fixpoint of `f` is a value, i.e. some function `x`, such that `f x = x`
- fixpoint finder `fix` takes a function as an arg and produces its fixpoint
- say that applying `fix` to `f` produces a fixpoint of `f` called `x`
- then applying `f` to its fixpoint, `x`, just gives `x` back, `f x = x`
- moreover, iterating `f` over its fixpoint `x` still gives `x`, `f (f x) = x`

Iterating a function over its fixpoint `x` just gives `x` back:

```hs
         f x = x
      f (f x) = x
   f (f (f x)) = x
f (… (f (f x)) …) = x
```

The goal is `fix f = f (fix f)`

```hs
f x   = x                      -- (1) x is a fixpoint of f
fix f = x                      -- (2) given f, fix produces its fixpoint x
fix f = f x                    -- (3) equality between (1) and (2) on x
fix f = f (f x)                -- (4) since x = f x
fix f = f (fix f)              -- (5) since f x = fix f by (3)
```

The fixpoint (finder) combinator, `Y` or `fix`, is then a function such that applying it to the function argument `f` gives back `f (Y f)`: `Y f = f (Y f)`. In fact, it produces an infinite number of applications of `f` on `Y f`, i.e. it iterates `f` over `Y f` (and `Y f` is a fixpoint of `f` so `f (Y f) = Y f`).

```hs
Y f = f (Y f)                   -- (1)
Y f = f (f (Y f))               -- (2) subst (Y f) with (f (Y f))
Y f = f (f (f (Y f)))           -- (3) since Y f = f (Y f)
Y f = f (… (f (f (Y f))) … )    -- (4) and so on, ad infinitum
```



TBC toward...

Y := λf. (λx.f(xx)) (λx.f(xx))
Y := λf. (λx.f(xx)) (λy.f(yy))
