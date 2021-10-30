# Infix type constructors, classes, and type variables

GHC allows `type constructors`, `classes`, and `type variables` to be `operators`, and to be written `infix`, very much like expressions. More specifically:

* A `type constructor` or `class` can be any non-reserved operator. *Symbols used in types are always like capitalized identifiers; they are never variables*. This is different from the lexical syntax of `data constructors`, which are required to begin with a colon.

* `data-declaration`, `class-declaration` and `type-synonym` declarations can be written infix; but parenthesised if you want further arguments, e.g.

```hs
type  a ✱ b = Sum a b
data  a ✪ b = Product a b
class a == b where ...
data (a ★ b) (x ≅ y) = ((a, b), (x, y))
type (a ✼ b) y = Either (a,b) y
```

* class-constraints, and types (in type-signatures) can be written infix,

```hs
x :: Int :*: Bool
f :: (a :==: b) => a -> b
```

* Backquotes work like they do for exprs, both for `type-ctors` and `type-vars`

```hs
type A¹ = Int `Either` Bool
type A² = Either Int Bool

type B¹ a = Int `a` Bool
type B² a = a Int Bool

-- Similarly, parentheses work the same
type C = (:*:) Int Bool
```

* Fixities may be declared for type-constructors or classes, just as for data-constructors. However, one cannot distinguish between the two in a fixity-declaration; a fixity declaration sets the fixity for a data constructor and the corresponding type constructor. For example, `infixl 7 T, :*:` sets the fixity for both type-constructor `T` and data-constructor `T`, and similarly for `:*:`.

* The function arrow `->` is `infixr` with fixity -1
