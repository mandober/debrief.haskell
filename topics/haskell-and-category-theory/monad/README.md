# Monads

- Monads provide a way of structuring functional programs.
- Monads are ideal for dealing with side-effects and modelling computation.


Computations modelled with monads

- logging with `Writer` monad
- shared, readonly, environment with `Reader` monad
- stateful computations: interleaving state with `State` monad
- exceptions
- continuations
- partial functions
- indeterminism


Each monad is specialized for a particular effectful computation tasks.
- The `Maybe` monad is used to signal computation failure
- the `Either` monad can do the same while also providing a detailed error message
- the list monad represents nondeterministic computation by returning multiple results
- the `IO` monad hosts effectful external communications
- the `Reader` monad may be used to provide access to a shared environment
- the `Writer` monad can accumulate and log the outputs
- the `State` monad can model stateful computations
- the `Cont` monad is the mother monad since it can represent other monads, and with that, almost every aspect of computation.


## Aspects of computation

Monads are used to model computation, especially the effectful aspects of computations that are very hard to manage correctly in a purely functional way. After all, it would seem that pure code cannot deal with side-effects and remain pure. Pure functions may only exert a single main effect and that is to return the computed value. Pure functions must also return the same output, given the same input.

Any other but the main effect is a side-effect. While the main effect is always dutifully documented in a function's signature, by enumerating the input and output types, the side-effects are mentioned only informally in the comments.

The big advantage of statically typed languages is being able to reason about any function just by inspecting its type signature. Type annotations and type signatures also act as a specification and provide documentation. However, the types are primarily used to provide a certain set of guarantees around the correctness of a program (function, module). The types alone should be sufficient to convey the purpose, and frequently they can also suggest the correct definition, of a function.

Types already restrict the range of valid values a variable can take, and with additional constraints brought on by an advanced type system (quantification, impredicativity, parametric polymorphism, type classes, functional dependencies, kinds, higher-kinded types, higher-ranked types, variance, roles, linearity, dependent typing, etc.) it frequently becomes possible to automatically infer a correct definition for a function, give only its type signature (see tools like `djinn` and `exference`).

As a brief example, consider the signature `f :: forall a. a -> a` and the corresponding function implementation. The pool of possible definitions is quickly exhausted leaving `f x = x` as the only reasonable (non-patological) definition. The types are forcing the correct definition to emerge.

For a more engaged example, consider the definition of a function with the following as its type signature:

```hs
f :: (((a -> b) -> r) -> r)
  -> ((a -> r) -> r)
  -> ((b -> r) -> r)
```


Many functional programming languages (ocaml, sml, lisp, miranda) fail to document all the action happening inside a function. These are largely strict FPLs that encourage purity but nevertheless permit unrestricted side-effects. Having no means or idea how to manage the possible effects, these languages simply ignore them, specifying nothing about them in the type signatureth at then looks as if the function were pure, thereby disseminating false advertisemnt and tricking the unexpecting clienets.

---

The types around the main effect do get documented, but the types describing the side-effects are left out.

This makes it hard to model a range of effectful computations, the large part of which are input-output communications.

How is then a pure function supposed to fetch the current time or a random number without breaking the purity constraints?
