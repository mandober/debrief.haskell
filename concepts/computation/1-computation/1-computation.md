# Haskell :: Concepts :: Computation :: 1. Computation

## Computation

Computation can be divided into pure and effectful computation. Pure computation is carried out by pure functions; pure computation equals pure functions, but it is not so simple with effectful computation.

The adjective "effectful" is usually attached to the noun "computation", most often occurring as the phrase *effectful computation*, by which we mean a computation that includes side effects.

The adjective "pure" is usually attached to the noun "function", most often occurring as the phrase *pure function*, by which we mean a computation that excludes side effects.

**Pure functions** are similar to mathematical functions - except the main one, they exhibit no other effects. Having no unintended effects means a function always behaves *deterministicaly* and *predictibly* - given the same inputs, it always returns the same output. This is the number one property of pure functions. Pure functions have no side effects means they do not influence their enclosing environment. So, no connecting to databases, no reading files or traversing directories, no FS interaction at all, in fact no I/O at all, no printing stuff on the console, no promopting users for input, no mutating global values. If it sounds fun, it's probably a side effect.

**Effectful computation**, on the other hand, is all about side effects. Side effects are necessary to get things done, purity isn't. So, why would a language insist on purity and how does a language remains pure in face of side effects?
