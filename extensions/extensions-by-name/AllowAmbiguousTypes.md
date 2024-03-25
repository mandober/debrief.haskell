# AllowAmbiguousTypes

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html

- `AllowAmbiguousTypes`
- since 7.8.1
- Allow type signatures which appear that they would result in an unusable binding.


Each user-written type signature is subjected to an *ambiguity check*. The ambiguity check rejects functions that can never be called. For example:

```hs
f :: C a => Int
```

The idea is there can be no legal calls to `f` because every call will give rise to an ambiguous constraint.

>The only purpose of the ambiguity check is to report functions that cannot possibly be called.

We could soundly omit the ambiguity check on type signatures entirely, at the expense of delaying ambiguity errors to call sites.

The language extension `AllowAmbiguousTypes` switches off the ambiguity check.


* Ambiguity can be subtle - consider this example which uses functional dependencies:

```hs
class D a b | a -> b where
  -- ...

h :: D Int b => Int
```

The `Int` may well fix `b` at the call site, so that signature should not be rejected.


* Also, the dependencies might be hidden.

```hs
class X a b where -- ...

class D a b | a -> b where -- ...

instance D a b => X [a] b where -- ...

h :: X a b => a -> a
```

Here the type of `h` looks ambiguous in `b`, but here's a legal call:

```hs
... (h [True]) ...
```

That gives rise to a `X [Bool] β` constraint, and using the instance means we need `D Bool β` and that fixes `β` via `D`'s fundep!


* Behind all these special cases there is a simple guiding principle.

```hs
f :: type
f = -- ...blah...

g :: type
g = f
```

You would think that the definition of `g` would surely typecheck! After all `f` has exactly the same type, and `g = f`.

But in fact the type of `f` is instantiated and the instantiated constraints are solved against the constraints bound by the signature of `g`.

So, in the case an ambiguous type, solving will fail.

For example, consider the earlier definition `f :: C a => Int`


In `g`'s definition, we'll instantiate to `C α` and try to deduce `C α` from `C a`, and fail.

In fact we use this as our definition of ambiguity:
>A type `ty` is ambiguous iff `(undefined :: ty) :: ty` fails to typecheck.

We use a very similar test for inferred types, to ensure that they too are unambiguous.


## Switching off the ambiguity check

Even if a function has an ambiguous type according to the "guiding principle", it is possible that the function is callable. For example:

```hs
class D a b where ...
instance D Bool b where ...

strange :: D a b => a -> a
strange = ...blah...

foo = strange True
```

Here strange's type is ambiguous, but the call in foo is OK because it gives rise to a constraint (D Bool beta), which is soluble by the (D Bool b) instance.

Another way of getting rid of the ambiguity at the call site is to use the TypeApplications extension to specify the types. For example:

```hs
class D a b where
  h :: b

instance D Int Int where ...

main = print (h @Int @Int)
```

Here a is ambiguous in the definition of D but later specified to be Int using type applications.

AllowAmbiguousTypes allows you to switch off the ambiguity check. However, even with ambiguity checking switched off, GHC will complain about a function that can never be called, such as this one:

```hs
f :: (Int ~ Bool) => a -> a
```

Sometimes AllowAmbiguousTypes does not mix well with RankNTypes. For example:

```hs
foo :: forall r. (forall i. (KnownNat i) => r) -> r
foo f = f @1

boo :: forall j. (KnownNat j) => Int
boo = ....

h :: Int
h = foo boo
```

This program will be rejected as ambiguous because GHC will not unify the type variables j and i.

Unlike the previous examples, it is not currently possible to resolve the ambiguity manually by using TypeApplications.


A HISTORICAL NOTE: GHC used to impose some more restrictive and less principled conditions on type signatures. For type `forall tv₁..tvₙ (c₁, ..., cₙ) => type` GHC used to require that each universally quantified type variable `tvᵢ` must be "reachable" from type, and that every constraint `cᵢ` mentions at least one of the universally quantified type variables `tvᵢ`. These ad-hoc restrictions are now completely *subsumed by the new ambiguity check*.
