# Singletons

The first paper about singletons:
Dependently Typed Programming with Singletons - Richard Eisenberg 
http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

## Misc info from SO

Consider the Peano-style natural numbers datatype:

```hs
data Nat = Z | S Nat
```

By using the language extension *DataKinds* this type is promoted to the type level. The data ctors `Z` and `S` are promoted to the type ctors `'Z` and `'S`.

With this we get that every natural number `n` (at the term-level) has a single and unique corresponding type (at the type level). So, `3` is `'S ('S ('S 'Z))`.

Eisenberg then defines (at the term-level) function `plus`, and at the type level, the type family `Plus`.

Eisenberg also defines a data type `SNat`:

```hs
data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
```

https://stackoverflow.com/questions/45235710/haskell-singletons-what-do-we-gain-with-snat

Q: Basically he only wraps the `Nat` type into a `SNat` constructor. Why is this necessary? What do we gain? Are the data types `Nat` and `SNat` not isomorphic? Why is `SNat` a singleton and `Nat` is not? In both cases every type is inhabited by one single value, the corresponding natural number?!

A: What do we gain? Hmm. The status of singletons is that of awkward but currently necessary workaround, and the sooner we can do away with them, the better. Let me see if I can clarify the picture. We have a data type `Nat`:

```hs
data Nat = Z | S Nat
```


The type `Nat` has run-time values which are indistinguishable at the type level. The Haskell type system currently has *the replacement property*, which means that in any well typed program, you may replace any well typed subexpression by an alternative subexpression with the same scope and type, and the program will continue to be well typed. For example, you can rewrite every occurrence of `if <b> then <t> else <e>` to `if <b> then <e> else <t>` and you can be sure that nothing will go wrong...as long as only the outcome of type checking is concerned.

The replacement property is an embarrassment. It's clear proof that the type system gives up at the very moment that meaning starts to matter.

Now, by being a data type for run-time values, `Nat` also becomes a type of type-level values `'Z` and `'S`. The latter live only in Haskell's type language and have no run-time presence at all: *type erasure*.

Although `'Z` and `'S` exist at the type level, it is unhelpful to refer to them as "types" since they don't have kind `*`, thus they cannot classify values, which is what types are all about.

There is no direct means of exchange between run-time and type-level `Nats`, which can be a nuisance. The paradigmatic example concerns a key operation on vectors:

```hs
data Vec :: Nat -> * -> * where
  VNil   :: Vec 'Zero x
  VCons  :: x -> Vec n x -> Vec ('Suc n) x
```

We might like to compute a vector of copies of a given element (perhaps as part of an Applicative instance). It might look like a good idea to give the type

```hs
vec :: forall (n :: Nat) (x :: *). x -> Vec n x
```

but can that possibly work?

In order to make `n` copies of something, we need to know `n` at run time: a program has to decide whether to deploy `VNil` and stop, or to deploy `VCons` and keep going, and it needs some data to do that.

A good clue is the `forall` quantifier, which is parametric:

> The forall keyword indicates that the quantified information is available only to types and is erased by run time.


Haskell currently enforces an entirely spurious coincidence between dependent quantification (what `forall` does) and type erasure for run time.

It doesn't support a dependent quantifier, but a *"not erased" quantifier*, which we often call "pi" (pi types). The type and implementation of `vec` should be something like:

```hs
vec :: pi (n :: Nat) -> forall (x :: *). Vec n x
vec 'Zero    x = VNil
vec ('Suc n) x = VCons x (vec n x)
```

where arguments in pi-positions are written in the type language, but the data are available at run time.

So what do we do instead?

> We use singletons to capture indirectly what it means to be a run-time copy of type-level data.

```hs
data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSuc  :: SNat n -> SNat (Suc n)
```

Now, `SZero` and `SSuc` make run-time data.

`SNat` is not isomorphic to `Nat`: `SNat` has kind `Nat -> *`, while `Nat` has kind `*`, so it's a type error to try to make them isomorphic.

There are many run-time values in `Nat`, and the type system doesn't distinguish them; there is exactly one run-time value (worth speaking of) in each different `SNat n`, so the fact that the type system cannot distinguish them is beside the point. The point is that each `SNat n` is a different type for each different `n`, and that GADT pattern matching (where a pattern can be of a more specific instance of the GADT type it is known to be matching) can refine our knowledge of `n`.

We may now write

```hs
vec :: forall (n :: Nat). SNat n -> forall (x :: *). x -> Vec n x
vec SZero    x = VNil
vec (SSuc n) x = VCons x (vec n x)
```

**Singletons** allow us to bridge the gap between run time and type-level data, by exploiting the only form of run-time analysis that allows the refinement of type information.

It's quite sensible to wonder if they're really necessary, and they presently are, only because that gap has not yet been eliminated.
