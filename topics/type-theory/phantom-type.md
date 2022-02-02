# Phantom type

https://wiki.haskell.org/Phantom_type

Normally, the set of type params, that are used on the RHS, are first declared on the LHS. A **phantom type** is a parametrized type one (or more) of whose type parameters doesn't appear on the right-hand side of the type declaration.

```hs
-- Regular type decl
data T a b c = A a | B a b | C a b c

-- Phantom type from Control.Applicative: `b` is a phantom TP
newtype Const a b = Const { getConst :: a }
```

`Const` is a phantom type, because `b` (phantom) type parameter doesn't appear on the LHS (it is declared but not used).

Phantom types are useful in a variety of contexts:
- `Data.Fixed` uses them with type classes to encode the precision being used
- with smart constructors or GADTs they can encode info about how and where a value can be used
- they may help avoid kind signatures
- with extensions, they're used for encoding bounds checks in the type system
- they can add a layer of type safety for types and DSL's
- help restrict what values functions can take
- enable encoding pre-conditions and post-conditions directly into the type

Since the values of type parameters in a phantom type may be unused, they are often used in combination with *the empty type*.

Phantom types are nearly always either `newtype` or `data`. It is possible to create *phantom type synonyms*, but they are usually useless: since synonyms are expanded at compile time, the phantom type param just gets discarded.


The `a` TP below is phantom, it just exists as a dummy parameter for `Foo` type. We can use `MkFoo` without ever requiring something of type `a`.

```hs
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

ghci> data Foo a = MkFoo
ghci> :set -XConstraintKinds
ghci> :set -XPolyKinds
ghci> :t MkFoo :: Foo Int
Foo Int
ghci> :t MkFoo :: Foo Bool
Foo Bool
ghci> :t MkFoo :: Foo Either -- requires -XPolyKinds where 'Foo' is defined
Foo Either
ghci> :t MkFoo :: Foo Monad  -- requires -XConstraintKinds
Foo Monad
```

The canonical ise case for phantom types is to "taint" the same data such that one case repr, e.g. sanitized data, while the other unsaniztized; or, repr users where `User 'Admin` repr a user with an admin token, while `User 'Norm` repr a normal user (this implies promoted ctors, *DataKinds*).


## A DSL case study

```hs
{-# LANGUAGE DataKinds #-}

data DoorState = Opened | Closed | Locked deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }
```

* The type `Door` contains a single field `doorMaterial` (UnsafeMkDoor "Oak")
* *DataKinds* creates both, the type `DoorState` and the kind `DoorState`
* Normally, `data DoorState = Opened | Closed | Locked` defines the type `DoorState` and the data ctors: `Opened`, `Closed`, `Locked`.
* with DataKinds, it also defines a new kind `DoorState` with type ctors `'Opened`, `'Closed`, `'Locked`
* Door type has a phantom param `s`. We can use `UnsafeMkDoor` without ever using anything of type `s`. (a better Door type would have the direct UnsafeMkDoor ctor hidden, exposing only smart ctors)
