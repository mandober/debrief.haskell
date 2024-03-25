# Nullary type class

- pragma `MultiParamTypeClasses` (implies `ConstrainedClassMethods`)
- since: 6.8.1
- ex deprecated pragma `NullaryTypeClasses`

> Since there are no available parameters, there can be at most one instance of a nullary class.

A nullary type class might be used to document some assumption in a type signature (such as reliance on the Riemann hypothesis) or add some globally configurable settings in a program. For example,

```hs
class RiemannHypothesis where
    assumeRH :: a -> a

-- Deterministic version of the Miller test
-- correctness depends on the generalised Riemann hypothesis
isPrime :: RiemannHypothesis => Integer -> Bool
isPrime n = assumeRH (...)

instance RiemannHypothesis where
    assumeRH = id
```

The type signature of `isPrime` informs users that its correctness depends on an unproven conjecture. If the function is used, the user has to acknowledge the dependence with `instance RiemannHypothesis where assumeRH = id`.
