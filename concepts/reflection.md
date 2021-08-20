# Reflection

Reflection is the capability of a PL to inspect its types at runtime, possibly reifying them pn demand.

The ability to perform type tests at runtime blurs the line between statically-typed and dynamically-checked languages. Recent developments in Haskell type system allow even programs that use reflection to themselves be statically typed, using a *type-indexed runtime representation of types called `TypeRep`*. As a result we can build dynamic types as an ordinary, statically-typed library, on top of `TypeRep` in an open-world context.

The issue of having functions that have type-specific, rather than type-parametric, behaviour, yet there is a need to have one name for a method common accross a set of types, were completely solved by introduction of type classes. Classes have made possible overloading, i.e. ad hoc polymorphism, but a range of other features as well: multi-parameter type classes, functional dependencies, constructors classes, implicit parameters, derivable classes, etc.

The `Typeable` class gives Haskell a handle on reflection: the ability to examine types at runtime and take action accordingly. Many PLs support reflection in some form, but Haskell is moving steadily towards an unusually statically-typed form of reflection, which sounds contradictory since reflection is all about dynamic (runtime) type tests.

- static types as formal method of proof
- static types as a design language for algorithmic code

And yet, there comes a point in every language at which the static type system simply cannot express what you want. As Leroy and Mauny put it "there are programming situations that seem to require dynamic typing in an essential way" [LM91]. Is there a way to introduce dynamic typing into a statically typed PL?

We motivate the need for dynamic typing and why it needs to work in an *open world of types*. Dynamic typing requires a *runtime test of type equality*, so some structure that represents a type, a *type representation*, must exist at runtime, along with a way to get the type representation for a value. We describe a type-indexed form of type representation, `TypeRep a` and explain how to use it for a *type-safe dynamic type test*. We show that simply *comparing type representations* is not enough, since in some situations we must also safely *decompose type representations*; supporting decomposition for type representations requires GADT-style *kind equalities*. Our key result is a way to build open-world dynamic typing as an ordinary statically-typed library (i.e. not as part of the trusted code base), using a very small (trusted) reflection API for `TypeRep`.

## 3. Dynamic types in a statically typed language
