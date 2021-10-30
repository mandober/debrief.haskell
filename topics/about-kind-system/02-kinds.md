# Kinds

With `TypeInType` extension, the kind/type of the kind `Type` is `Type`, i.e. `Type :: Type`, that is, types and kinds are equated. However, types still have kinds, but kinds also have kinds (or types since they're the same at that level)

Haskell 98 standard has has 2 kinds:
- `κ := Type | κ₁ -> κ₂`
- the base kind `Type` (deprecated `*`) classifies inhabited types:
  - base types: Int, Integer, Word, Char, Float, Double
  - nullary type ctors: `Bool`, `Ordering`, etc.
  - saturated type ctors: `Maybe a`, `Either a b`, `a -> b`, etc.
  - `κ⁰ = Type`
- the kind function `κ₁ -> κ₂` classifies unsaturated type ctors:
  - unary type ctors
    - `IO, Maybe, [] :: Type -> Type`
    - `κ¹ = Type -> Type`
  - binary type ctors
    - `(->), (,), Either :: Type -> Type -> Type`
    - `κ² = Type -> Type -> Type`
  - n-ary type ctors

GHC with extensions introduces more kinds:
- `TYPE` is the kind universe, encompassing all other kinds
- `#` is the kind of the unlifted primitives types
- `Type` is the kind of base types, nullary type ctors, saturated type ctors
- `κ -> κ` is the kind of unsaturated type ctors of diff arities
- `Constraint` is the kind of contexts (fat arrow's LHS)
  - `Eq, Ord, Show :: κ -> Constraint`
  - `Eq a, Ord a, Show a :: Constraint`
- promoted types introduce new custom kinds
  - type ctor `[]` also becomes the kind `[]`
    - the kind `[]` classifies only 2 type ctors, both uninhabited,:
      - '[]  :: forall a. a
      - '(:) :: forall a. a -> [a] -> [a]
