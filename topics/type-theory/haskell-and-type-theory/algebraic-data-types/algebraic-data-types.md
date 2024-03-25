# Algebraic Data Types

All programming languages have a set of basic, the most primitive types, that the compiler knows about, because these language primitive types correspond to machine primitive types. Such types are called *the base types*. Each such type almost always has a literal form.

If we were to design a programming language, the first thing we should do is define all the base types, after which we can start coming up with the ways to construct new, more complicated, types, that combine the base types, using them as building blocks.

Haskell uses algebraic constructs such as unit, sum, product, exponentiation as a way to build new data types from the base and/or previously designed data types. This algebra-like constructions are precisely the reason Haskell types are called Algebraic Data Types (ADT).

Haskell makes scarce the true machine primitive types, but makes available their lifted variants; each machine-dependent primitive type is extended with a special value, called bottom, that represents a diverging computation of that type. Therefore, by default exposure, all types are indeed ADT; even the integers, which may be understood as an enormous sum type, with a value of type Int being either 0, or 1, or 2, or etc...similarly to all sum types that represent disjoint set unions.


Types correspond to sets, but types also respect different properties related to misc structures. For example, is there an equivalence relation on Haskell types? An equivalence relation (denoted here by `≈`) on types would mean they are reflexive, symmetric and transitive.

Types are reflexive fo sho. `Int ≈ Int`, `Float ≈ Float`, so generally, `a ≈ a`. What about symmetry? Symmetry states that is `a` is `R`-related to `b` iff `b` is `R`-related to `a`, denoted by `a ~ b <=> b ~ a`. There's no meaningful symmetric relation of concrete type e.g. between Float and String, and it is also questionable what would be the situation with type vars, `a ≈ b ≡ b ≈ a`. However, it is surprising that types don't even support transitivity. Consider this:

```hs
a = Nothing :: Maybe a
b = Nothing :: Maybe Int
c = Nothing :: Maybe Char

-- comparing these 3 values gives
a == b  -- True
a == c  -- True
-- still, b /= c
b == c  -- ERROR: cannot unify Int and Char
```

Even though, `a == b` and `b == c`, it is not true that `b == c`, which is an intransitive relation, `¬(∀abc. a = b ∧ b = c => a = c)`.

Hand at heart, this happens because in the case of `a` value, the type is actually `forall a. Maybe a`, which means the type can be any type as determined by the call site. So the first call site, `a == b`, in fact makes a comparison between `forall a. Maybe a` and `Maybe Int`, such that `a` is instantiated at the `Int`, yielding a comparision between `Maybe Int` and `Maybe Int`, which evaluates to `True`. Similarly, in the second case, when `a` type var is instantiated at `Char`. Two `Nothing`'s of the same type, `Maybe Char`, get compred, which evals in `True` as well.
