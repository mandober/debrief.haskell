# Finite set type

The `Fin` or `Ordinal` is a data type known as a *finite set* or *ordinal*, that represents "natural numbers below `n`". `Fin n` is a sequence `[0..n-1]`.

This type is useful at the type level, where it's used to implement vector indexing. To look up a value in a vector by an index, we use the operator `!!`, the same one used for list indexing. However, unlike list indexing, where an index may trigger the out-of-bounds error, with vectors we can do better since we know its length statically. The key is to come up with a data type that will represent natural numbers bounded from above. Traditionally, in dependently typed programming, such type is called `Fin`, for a Finite Set. The `Fin` type is indexed by a natural number `n`. The type `Fin n` contains `n` values.

A vector is a GADT `Vec (n :: Nat) a`, where `n` is a promoted `Nat` (Peano-style naturals), and `a` is the type of vector elements. For example, `Fin Z` has no proper inhabitants, and `Fin (S (S Z))` has exactly two inhabitants, `Z` and `S Z`.

If `n` is greater than 0, `Fin n` has 0 as its inhabitant. If `k :: Fin n`, then `k+1 :: Fin n` might fail (in case `k = n-1`), but `k+1 :: Fin (n+1)` always holds. Using this observation, we can implement the Fin as follows:

```hs
data Fin (n :: Nat) where
  FZ ::          Fin (S n)
  FS :: Fin n -> Fin (S n)
```

Vector indexing is 0-based, so the valid indices for a vector of 10 elements are in the list `[0..9]`. As long as vector is nonempty, the lower bound of indices is 0; the upper bound is a vector's length minus 1.

We just need to connect vector's length `n` and `Fin n`. A Nat `n`, passed as a singleton (to e.g. `replicate`) will construct a vector with `n` elements, and the `Fin n` type will also have `n` inhabitants, corresponding to the number of vector's elements.

`Fin n` is a type with `n` values and a function in `Fin n -> x` is like an `n`-tuple of `x`-values. It's like a flat way of writing the exponential `xⁿ`.




## References

https://pigworker.wordpress.com/2015/06/06/hasochistic-containers-a-first-attempt/

https://web.archive.org/web/20171028025035/https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell
