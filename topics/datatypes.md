# Datatypes

## Builtin datatypes

Haskell's datatypes can be classified as builtin (intrinsic) datatypes, i.e. the datatypes who the compiler has intimate knowledge of. All primitives are builtins, in both their forms. Their native form is referred to as *unlifted*, and machine-dependent types, such as integer (e.g. 64-bit integers on x86_64 arch) is denoted with a suffixed "magic hash", so `Int#` is a machine-dependent (unlifted) integer datatype. The "magic hash" also denotes the kind all these primitive unlifted types share, the kind `#`. The unlifted types are not normally available in the surface syntax, instead, their lifted versions are. The lifted type of integers is the type (the set) `Int` consisting of the set of machine-dependent integers united with the bottom, `Int = Int# ∪ ⊥`. It is similar for other machine primitives.

## Bottom

In fact, each and every type in Haskell (sans these machine primitives) is lifted, meaning it is extended with bottom. Bottom is a strange type/value; it is a value of every type. Actually, bottom represents the type/value of diverging computations. A function that proceeds to calculate something forever, an infinite loop, an error/exception all have the same value, i.e. bottom. Bottom can be explicitly denoted by `undefined`. There is no way to detect whether a function (computation) will result in bottom - that would mean that the answer to the halting problem is positive, and not negative like proved (briefly, it is a question of existence of a program that can detect whether another program halts, producing a result, or loops indefinitely).

## Privileged datatypes

Considering lifted (normal) types, we can divide them into privileged and privileged, the former being the types the compiler is intimately familiar with. Besides, the scalar types (Int, Float, Double, Char and such), they also include compound types, some of which are regularly encountered:
- the function type, `((->) a b)`
- the list type, `[] a`
- the n-tuple type that has many forms
  - 2-tuple (pair) form: `(,) a b`
  - 3-tuple (pair) form: `(,,) a b c`
  - 4-tuple (pair) form: `(,,,) a b c d`
- the disjoint sum type (?)

The pair is the canonical form of product types. The disjoint sum type is the canonical form of sum types (whose internal form may be `(a|b)`, no idea), but which, on the surface, is represented by the Either type, as `Either a b` (although Either is not special, I think).

If these privileged types were allowed to be user defined, maybe they would look similar to this:

```hs
data (->) a b   = (\a -> b)
data []   a     = (:) a ([] a)
data (,)  a b   = (a,b)
data (,,) a b c = (a,b,c)
data (|)  a b   = (a|b)
```

The function type is the most complicated, have no idea as to its repr. In any case, the type ctor is the arrow `->` and because it has two type params, it can have 3 kinds depending of its saturation:
- (->) :: * -> * -> *
- ((->) r) :: * -> *
- ((->) r a) :: *

The list type is exotic because the brackets can surround a type, `[Int]`, and they also act as delimiters, in that they can alleviate the need to separate things with parens, `A [C e]` instead of `A ([C e])`.

The pair, `(,) a b`, is similar to the list in that the parens can surround the types, `(a,b)`. Other tuples act similarly. The type ctor of the pair is `(,)`, that is, including parens (?); they are not used to contain the infix operator, like in the case of the function type ctor, (->), but are integral part?Shouldn't it be `((,))` in that case? God knows. The pair type ctor also has 3 kinds depending of its saturation:
- (,) :: * -> * -> *
- ((,) a) :: * -> *
- ((,) a b) :: *

The disjoint sum is represented by the syntactically correct `Either a b` type, but (I think) its internal repr may be `(a|b)`. Anyway, Either is the canonical form of sum types.

## Constructible datatypes

All other datatypes are unprivileged in that, if they were user-definable they would act correspondingly to their builtin counterparts [sure?].

The user (custom) datatypes are (properly) introduced with the `data` keyword. Using this keyword is the only way to introduce a brand new datatype. The `newtype` keyword is intended for wrapping an already existing type so it can be managed more easily, and the `type` keyword introduces a (shorter) alias for a (long) type signature. Only types declared with data and newtype keywords can be made class instances. Only a type with a single data ctor can be wrapped in a newtype.

The `data` keyword is the only one that can declare a (brand) new type, and it can do so in two related approaches. The common approach is ADT declaration (identified with `=` sign), but the pragma-enabled GADT declaration (identified with `where` keyword) is actually a more clear way to introduce a datatype.

```hs
-- plain ADT declaration types the data ctors in a roundabout way
data Exp a = Var a
--   ^^^^^1  ^^2 ^3
-- data ctor type is (2 :: 3 -> 1): Var :: a -> Exp a

-- GADT declaration types the data ctors in a straightforward fashion
data Exp a where  Var :: a -> Exp a
-- data ctor type Var :: a -> Exp a
```


## Algebraic datatypes

The constructible datatypes are algebraic datatypes, most common of which are *sum types* and *product types*, but others exist, such as *function types*, exponential types, quotient types, dependent types, etc.

> A general algebraic data type is a *possibly recursive sum type of product types*.

Each data ctor tags a product type to separate it from others, or, if there's only a single data ctor, the data type _is_ the product type.

The set of type parameters of the data constructor `C` (of the type `T`) is the *subset* of the type constructor's set of type parameters; e.g. `Either a b` vs `Left a`, that is, the data ctor "Left" has a type param `a` that is a *factor* of type ctor's type params, `a` and `b`.

A nullary type ctor corresponds to the empty product.

If a datatype is recursive, the entire *sum of products* is wrapped in a recursive type, and each data constructor also rolls the datatype into the recursive type.
