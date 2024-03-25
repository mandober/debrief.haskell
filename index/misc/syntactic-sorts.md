# Syntactic sorts

- term-level vs type-level vs kind-level
- types classify terms; terms are classified by types
- all terms are classified by types
- each term belongs to a particular type
- kinds classify types; types are classified by kinds
- each type belongs to a particular kind
- `TypeInType`: kinds and types are the same:
  `:k Type` replies `Type :: Type`
- only the kind `Type` is inhabited (with ground types)
- kinds other than the kind `Type` are uninhabited
- a type of the kind `Type` may be inhabited or uninhabited
- Haskell 98 had only two kinds, κ and κ -> κ (with κ = `Type`)
- `DataKinds` extension adds ADT promotion: the type ctor becomes the new kind `κ`, and the data ctors become (uninhabited) type ctors of the kind `κ`, prefixed with a tick in case of ambiguity (`True` vs `'True`)
