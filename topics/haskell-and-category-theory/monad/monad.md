# Monads

## Pure functions

Generically, pure functions are typed as `a -> b` [^1].

[^1]: The function type `a -> b` does not correspond to any sensible function in Haskell, and it should be interpreted as a metatype denoted in a metalanguage (that is used to discuss Haskell as the object language). There, it is actually a type scheme, perhaps better denoted by `α -> β`, meaning that the input type `α` and the output type `β` may be any types, e.g `β` may in fact be `m b` in the object language. And `β = m b` looks more sensible than `b = m b` (interpreted algebraically rather than recursivelly).

The type `a -> b` models pure functions, i.e. the type signature `a -> b` models the type signatures of pure functions.

And the problem with pure functions is that you can't (can) accomplish shit relaying on them alone. ("can or can't" - whichever way it goes, you're fucked).



Monads are effectful computations (functions).

Compared to pure functions, `a -> b`, monadic functions take the same input `a`, and return the same output `b` only augmented with, or embellished in, some additional context `m`, so their return type is actually `m b`.

- `a -> b`   pure functions
- `a -> m b` monadic functions, a.k.a Kleisli arrows

## Kleisli arrows

Monadic functions are properly called *Kleisli arrows*, `a -> m b`.

## Kleisli composition

The Kleisli composition is about composing the Kleisli arrows. Obviously, we can compose pure functions, `a -> b` and `b -> c`, as long as they are compatible (i.e. the codomain of one matches the domain of another).
