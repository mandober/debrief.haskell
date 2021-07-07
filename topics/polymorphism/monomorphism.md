# Monomorphism


## Monomorphic types

* Monomorphic types are fully saturated, completely instantiated, fully resolved, fully applied, final, concrete, types.

* Polymorphic types become monomorphic by consistent substitution of their params.

## Monomorphism restriction

The **monomorphism restriction** is a counter-intuitive rule in Haskell type inference, triggered with functions without a type signature, in which case this rule will fill the *free type params* with specific types according to the *type defaulting rules*.

The resulting type signature will always be less polymorphic than expected, so often this results in the compiler throwing type errors in situations where you expected it to infer a perfectly reasonable type for a polymorphic expression.

A simple example is `plus = (+)`. Without an explicit signature for `plus`, the compiler will not infer the desired type `(+) :: (Num a) => a -> a -> a`, but will apply the defaulting rules, inferring the type as `plus :: Integer -> Integer -> Integer`. If this `plus` is then applied to the expression like `plus 3.5 2.7`, GHCi will produce the somewhat misleading error: *No instance for (Fractional Integer) arising from the literal '3.5'*.

```hs
(+) :: (Num a) => a -> a -> a

plus = (+)
-- inferred in a compiled module as
plus :: Integer -> Integer -> Integer

-- inferred in GHCi as
plus :: (Num a) => a -> a -> a
```

By default, the monomorphism restriction is
- **turned on** in compiled modules
- turned off in GHCi (since GHC 7.8.1)
- override these defaults wiyh `-XMonomorphismRestriction` language pragma




## References

https://wiki.haskell.org/Monomorphism
