# Vector indexing

To look up a value in a vector by an index, we implement the operator (!!), the same one used for list indexing. However, unlike list indexing, where an index may trigger the out-of-bounds error, with vectors we can do better since we know its length statically.

The key is to come up with a data type that will represent natural numbers bounded from above. Traditianally, in dependently typed programming, such type is called `Fin` (short for "finite set"):

```hs
data Fin :: Nat -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)
```

The `Fin` type is indexed by a natural number `n`. The type `Fin n` contains exactly `n` values, corresponding to the numbers `0` to `n − 1`.

This GADT tends to be a bit harder to understand than Vec because (unlike Vec), you cannot tell the type of a Fin just from the value. For example, the value `FS FZ` can have types `Fin 2` and even `Fin 10`, only not `Fin 1` (using decimal notation instead of unary notation for Nats for simplicity).

Lets understand it better by tracing how can we assign a type to `FS FZ`:

Suppose we are checking to see whether  `FS FZ :: Fin (S Z)`
- the 2nd equation says    
  `FS :: Fin n -> Fin (S n)`  thus, for `FS FZ :: Fin (S Z)`, we inst. `FS` at
  `FS :: Fin Z -> Fin (S Z)`  with `n ~ Z`
- Now check that               `FZ :: Fin Z`
- However, this fails, because `FZ :: Fin (S n)`
- that is, `FZ`'s type index must not be `Z`
- We accordingly reject `FS FZ :: Fin (S Z)`


FS FZ :: ???                            type assignment
FS FZ :: Fin (S Z)                      <checking...>
FS :: Fin n -> Fin (S n)                `n ~ Z` so instantiate FS at the type
FS :: Fin Z -> Fin (S Z)                check that the next one holds:
FZ :: Fin Z                             fails because
FZ :: Fin (S n)                         FZ's type index must not be Zero
FS FZ :: Fin (S Z)                      <rejected!>


* Now say we're checking `FS FZ :: Fin 5`. This proceeds as above, but in the end, we check `FZ :: Fin 4`. The number 4 is indeed the successor of another natural, and so `FZ :: Fin 4` is accepted, and thus so is `FS FZ :: Fin 5`.


Following this logic, we can see how `Fin n` has precisely `n` values.

> As a type whose values range from `[0 .. n − 1]`, the `Fin n` is the perfect index into a vector of length `n`.

```hs
(!!!) :: Vec a n -> Fin n -> a
vec !!! fin =
  case (fin, vec) of  -- (1) reverse order due to laziness (see below)
    (FZ, x :> _)  -> x
    (FS n, _ :> xs) -> xs !!! n
```

GHC comes with a pattern-match completeness checker [Karachalias et al. 2015] that marks this case as complete, even without an error case. To understand why, we follow the types. After matching fin against either FZ or FS n, the type checker learns that n must not be zero-the types of both FZ and FS end with a Succ index. Since n is not zero, then it cannot be the case that vec is VNil. Even though the pattern match includes only :>, that is enough to be complete.

### Lazyness and pattern matching

We proceed to explore the pattern-match reversal at (1). Haskell is a lazy language, which means that variables can be bound to diverging computations, denoted by `⟘`.

When matching a compound pattern, Haskell matches the patterns left-to-right, meaning that the left-most scrutinee (`fin`, in the case of `!!!` above) is evaluated to a value and then inspected before evaluating later scrutinees, namely `vec`. Imagine matching against `vec` first: it is then conceivable that `vec` would be `VNil` while `fin` would be `⟘`. This is not just theoretical; witness the following function:

```hs
lazinessBites :: Vec a n -> Fin n -> String
lazinessBites VNil = "empty vector"
lazinessBites      = "non-empty vector"
```

If we try to evaluate lazinessBites VNil undefined, that expression is accepted by the type checker and evaluates handily to "empty vector". If we scrutinize vec first, then, the completeness checker correctly tells us that we must handle the VNil case. On the other hand, in the implementation of !!! with the pattern match reversed, we ensure that fin is not ⊥ before ever looking at vec and can thus be sure that vec cannot be VNil.
