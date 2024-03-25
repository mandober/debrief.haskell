# Define Kinds Without Promotion

Proposal
- title: Define Kinds Without Promotion
- author: Iavor Diatchki
- date-accepted: 2018-09-12
- implemented: 9.6
- ticket-url: https://gitlab.haskell.org/ghc/ghc/-/issues/6024
- ext-name: `TypeData`
- ext-related: DataKinds

GHC Proposal **#106** has been implemented, introducing a new language extension __`TypeData`__. This extension permits type data declarations as a more fine-grained alternative to `DataKinds`.


## Allow defining kinds alone, without a datatype
https://gitlab.haskell.org/ghc/ghc/-/issues/6024

Sometimes we want to define a kind alone, and we are not interested in the datatype. In principle having an extra datatype around is not a big problem, but the constructor names will be taken, so they cannot be used somewhere else. A contrived example:

```hs
data Code = Unit | Prod Code Code

data family Interprt (c :: Code) :: Type
data instance Interprt Unit       = Unit1
data instance Interprt (Prod a b) = Prod1 (Interprt a) (Interprt b)
```

We're only interested in the constructors of the data family `Interprt`, but we cannot use the names `Unit` and `Prod` because they are data ctors of `Code`.
The suggestion is to allow defining:

```hs
data kind Code = Unit | Prod Code Code
```

Such that `Code` is a kind, and not a type, and `Unit` and `Prod` are types, and not data ctors.

Note: using `data kind` instead of just `kind` means the word `kind` does not have to be a reserved keyword.

We could also have data types that should not be promoted by introducing the `data type` construct.

```hs
data K

data type T = K
```

But I don't see a need for this, as the fact that the `K` constructor is promoted to a type doesn't prevent you from having a datatype named `K`.


## GHC manual :: Language extensions :: TypeData

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_data.html

- `TypeData`
- Since: 9.6.1

Allow **type data declarations**, which define constructors at the type level.

This extension facilitates type-level (compile-time) programming by allowing a type-level counterpart of data declarations, such as this definition of type-level natural numbers:

```hs
type data Nat = Zero | Succ Nat
```

This is similar to the corresponding data declaration, except that the constructors `Zero` and `Succ` it introduces belong to the type constructor namespace, so they can be used in types, such as the type of length-indexed vectors:

```hs
data Vec :: Type -> Nat -> Type where
  Nil  :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)
```

>`TypeData` is a more fine-grained alternative to the `DataKinds` extension, which defines all the constructors in all data declarations as both data constructors and type constructors.

A type data declaration has the same syntax as a data declaration (either an ordinary ADT or GADT) only prefixed with the keyword `type`. It cannot contain a datatype context (even with the depracated `DatatypeContexts`), labelled fields, strictness flags, or a `deriving` clause.

The only constraints permitted in the types of constructors are *equality constraints*, e.g.:

```hs
type data P :: Type -> Type -> Type where
  MkP :: (a ~ Natural, b ~~ Char) => P a b
```

Because type-data-declarations introduce type ctors (and a kind), they do not permit ctors with the same names as types, so the following is invalid:

```hs
type data T = T     -- Invalid
```

The compiler will reject this declaration, because the type constructor `T` is defined twice (as the datatype being defined and as a type constructor).

The main type constructor of a type-data-declaration can be defined recursively, as in the `Nat` example above, but its ctors may not be used in types within the same mutually-recursive-group of declarations, so the following is forbidden:

```hs
type data T f = K (f (K Int))  -- Invalid
```
