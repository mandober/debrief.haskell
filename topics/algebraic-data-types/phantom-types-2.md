# Phantom types

In an ADT-style type declaration, on the lhs, following the name of the type ctor, the type parameters are listed, by default all of the kind `Type`. 

```hs
data La a s = Na | Ca a (La a s)

-- >>> :i La
type role La nominal representational phantom
type La :: forall {k}. Type -> k -> Type
data La @{k} a s = Na | Ca a (La @{k} a s)
```

Bar the elaboration given by the user, GHC almost always infers the kind `Type`.

The `StandaloneKindSignatures` pragma allows us to elaborate the type/kind signature on a separate line (similarly to the way functions signatures are given), except the kind signatures are introduced with the `type` keyword (otherwise used to declare a type alias).

Normally, it's not necessary to elaborate the involved types that much, as GHC is capable to infer them, and GHCi displays the (inferred or explicit) kind signatures in the queries about the data types (`:t` and `:i`).

to be explicit about the kind signatures, however some situations require it.


```hs
type List (a :: Type)
data List a = Nil | Cons a (List a)

data List a where
  Nil :: List a
  Cons :: a -> List a -> List a
```

Phantom types may be used to tag data.
  By declaring an extra type parameter on the lhs
  of a data definition, 
  and not referring to it on the rhs, 
  that type param becomes what is called a phantom type.

  We may create a new data type 
