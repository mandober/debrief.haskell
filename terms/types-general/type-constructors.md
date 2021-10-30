# Type constructors

The left side of a datatype declaration consists of the type ctor (which also acts as a type's name) and any number of type parameters.

The left side of a datatype declaration has the same form regardless whether the type being declared is a sum or product type. It contains the type name and declares all the type param that are to be used on the right side of a declaration.

```hs
-- datatype declaration: nullary data ctor
data T0 = ...
-- datatype declaration: unary data ctor
data T1 a = ...
-- datatype declaration: binary data ctor
data T2 a b = ...

-- datatype declaration: product datatype
data P2  a b = P2  a b
data (,) a b = (,) a b

-- datatype declaration: sum datatypes
data S0     = L   | R
data S1 a   = L a | R
data S2 a b = L a | R b
```

The type params determine the arity of type ctor. A type ctor is `n`-ary, where `n` is the number of its type params. No type params make a type ctor nullary (e.g. `Bool`). A single type param makes a type ctor unary (e.g. `Maybe a`, `[] a`). Two type params make a type ctor binary (e.g. `Either e a`, `(->) r a`, `Reader r a`, etc.).

Type ctors with two or more type params have *polyadic* arity (1-ary type ctor is also called monadic, but we better ignore that). Only polyadic type ctors can be partially applied.

The important thing in a datatype declaration is the number of type params and the order in which they are listed. These two things determine the different forms a type ctor can have. A type ctor with `n` type params has `n + 1` possible forms, each with a corresponding kind.

```hs
-- datatype with a binary type ctor
data Either e a = Left e | Right a

-- the kind of the unsaturated type ctor
Either :: * -> * -> *
-- the kind of the semi-saturated type ctor
Either e :: * -> *
-- the kind of the fully saturated type ctor
Either e a :: *
```

The forms a type ctor can find itself in depend on its arity and saturation. A nullary data ctor can only ever have one form, the form of the bare type ctor. A unary type ctor has two forms: in its unsaturated form (as bare type ctor) it has a kind `* -> *`, but when fully applied, its kind collapses to `*`. The saturation of a polyadic type ctor varies from unsaturated to fully saturated, with different saturation configurations in between.

Most type classes are single-parameter classes, which corresponds to properties in a logic system. With types as sets, a set `A` has the property `P` if `∀a ∈ A. P(a)` holds. A class with two parameters is a multi-param class (needs enabling the `MultiParamTypeClasses` pragma), which corresponds to relations in logic: a relation `R` between two sets `A` and `B` is a set of ordered pairs, `R = { (a,b) ∈ R | a ∈ A ∧ b ∈ B }`.

Multi-param classes relate two distinct type ctors. Single-param classes 



```hs
class Show (a :: *) where
class Functor (a :: * -> *) where
class Bifunctor (a :: * -> * -> *) where
```

The type ctor and its type params, in various forms, are subject to being made instances of classes. Each class specifies the particular kind a type ctor must have for it to become its instance.

Basic classes (`Eq`, `Show`, etc.) require that a type ctor has the kind `*`. So, any type, that is, any type ctor can be made an instance of these classes; it just needs to be in its fully saturated form (`Int`, `Bool`, `Maybe Int`, `Either String Int`, `((->) r a)`, etc.)

Algebraic classes (`Functor`, `Monad`, etc.) require that a type ctor has the kind `* -> *`. This means that nullary type ctors (`Int`, `Bool`) can never be instances of such classes.






```hs
-- function datatype declaration
data (->) a b = (->) a b

-- pair datatype declaration
data (,) a b = (,) a b
```








## References

https://en.wikipedia.org/wiki/Type_constructor
