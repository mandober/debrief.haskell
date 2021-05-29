# Open Union

http://okmij.org/ftp/Haskell/extensible/

*Extensible effects*, as well as *extensible interpreters* (Liang et al. `Monad Transformers and Modular Interpreters` and later Swierstra's `Data types à la carte`) all rely on *typed open unions*, or *open type-indexed coproducts*. An early compelling case for *open unions* are *extensible exceptions*, which have been part of Haskell for many years (S.Marlow, `Haskell Workshop 2006`).

To permit throwing exceptions of arbitrarily many types, the thrown exception value is an open union (see `SomeException` in `Control.Exception`). Raising an exception injects a particular exception value into the open union. When we handle an exception, we project: check if the thrown exception is of a particular desired type (extensible effects operate in the same manner, supporting, in addition, the resumption of an exception).

An *open union* should let us inject a value of any type into the union, and project a type from the union; that is, allow us to find out if the union value was previously injected with a particular specific type. These operations are familiar from OOP as *downcast* and *upcast*.

The open-union type of exceptions, `SomeException`, gives no indication of possible summands, that is, which particular exception types may be in the union. Therefore, Haskell cannot ensure that a program handles all exceptions that could be raised during its execution.

To do better, the type of an open union should be annotated with the set of possible summands. The injection function will add the type of the injected value to the union type, unless it was there already.

As always with types, the type of the open union is an approximation for the type of the value therein. Consider the simplest union `Either Int Bool`: at run time, the union value is either a single `Int` or a single `Bool`. The union type is an approximation: we cannot generally determine at compile-time the specific type of the value stored in the union. However, we can be sure that it's not a `String`, so we know not to attempt projecting a String from an `Either Int Bool` union at the compile-time for that would be a RT error.

A type-annotated union is called a **type-indexed co-product**.

## Either non-example

The `Either a b` datatype is the simplest example of typed unions, but it's non extensible. The constructors `Left` and `Right` are injections, and projections are realized via pattern-matching:

```hs
prj :: Either a b -> Maybe a
prj (Left x) = Just x
prj _        = Nothing
```

The type checker prevents us from injecting a value of a type other than `a` or `b` into `Either a b`, hence restricting injection to values that participate in the union. And we can only project at types `a` and `b` - `Either a b` is a union of exactly two types, and thus not extensible. Furthermore, it is not abstract: we must know the exact structure of the union in order to choose the proper injection, i.e. either `Left` or `Right`. The type `Either Int Bool` is different from `Either Bool Int`, although they are isomorphic.

## Union interface

Heeding the drawbacks of `Either`, we define the interface for open unions as:

```hs
data Union (r :: [*]) -- abstract

type family Member (t :: *) (r :: [*]) :: Constraint

inj :: Member t r => t -> Union r
prj :: Member t r => Union r -> Maybe t

decomp :: Union (t ': r) -> Either (Union r) t
```

The union type `Union r` is tagged with `r`, which is a list (for the lack of type-level sets) of summands of the kind `[*]` (list of types).

The injection `inj` and the projection `prj` ensure that the type `t` to inject or project must be a member of the list `r`, as determined by the type-level function `Member t r`.

The function `decomp` performs the orthogonal decomposition. It checks to see if the union value given as the arg is a value of type `t`. If so, the value is returned as `Right t`. Otherwise, we learn that the received union value does not in fact contain the value of type `t`. We return the union, adjusting its type so that it no longer has `t`.

The function `decomp` thus decomposes the open union into two orthogonal *spaces*: one with the type `t` and the other without.

The decomposition operation, which shrinks the type of open unions, is the crucial distinction of our interface from the previous designs. It is this decomposition operation, used to 'subtract' handled exceptions/effects, that ensures all effects are handled.

The constraint `Member t r` may be seen as the interface between `inj` and `prj` on one hand, and `decomp` on the other hand: for each injection or projection at type `t` there shall be a `decomp` operation for the type `t`.

This basic interface of open unions has several variations and implementations.

One is `OpenUnion1.hs`, presented in the Haskell 2013 extensible-effects paper.It is essentially the one described in Appendix C of the full `HList` paper, published in 2004. In the version for extensible-effects, the summands of the open union have the kind `* -> *` rather than `*`. This implementation uses `Dynamic` as the open union. Therefore, all operations take constant time.
