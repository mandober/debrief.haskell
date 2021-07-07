# Phantom types

https://blog.jle.im/entry/introduction-to-singletons-1.html

Phantom types in Haskell are a very simple way to add an extra layer of type safety for custom types. It helps you restrict what values functions take and encode pre-conditions and post-conditions directly into your types.

For example, the `a` TP is a phantom type because there's nothing of type `a` on the LHS of the data type. It just exists as a dummy parameter for the Foo type. We can use MkFoo without ever requiring something of type `a`.

```hs
data Foo a = MkFoo

ghci> :t MkFoo :: Foo Int
Foo Int

ghci> :t MkFoo :: Foo Bool
Foo Bool

ghci> :t MkFoo :: Foo Either  -- requires -XPolyKinds where 'Foo' is defined
Foo Either

ghci> :t MkFoo :: Foo Monad   -- requires -XConstraintKinds
Foo Monad
```

One use case of phantom type parameters is to prohibit certain functions on different types of values and let you be more descriptive with how your functions work together.

The canonical examples of phantom TPs is to tag data, e.g. as sanitized vs unsanitized (`UserString 'Sanitized` vs. `UserString 'Unsanitized`), or paths as absolute or relative (`Path 'Absolute` vs. `Path 'Relative`).

## Example

For example, here is how to code a simple DSL for a type-safe door:

```hs
{-# LANGUAGE DataKinds #-}

data Door (s :: DoorState) = MkDoor { doorMaterial :: String }

data DoorState = Opened | Closed | Locked deriving (Show, Eq)
```

* The `Door` type contains a single field `doorMaterial` describing the material the door is made of, e.g. `MkDoor "Oak"`.

* *DataKinds* extension will automatically create both, the `DoorState` type and the `DoorState` kind (as well as the `Door` type and the `Door` kind).

* Normally, `data DoorState = ...` defines the type `DoorState` and the data ctors Opened, Closed, Locked. However, with *DataKinds*, this also defines a new kind `DoorState`, with type ctors `'Opened`, `'Closed`, `'Locked`.

* `Door` has a phantom TP `s` whose kind is `DoorState`. We don't actually have any values of type `s`, nor is it possible to have a value of type `'Closed` or `'Opened`, etc.


Use *TypeApplications* extension to write this more conveniently.

```hs
ghci> :t UnsafeMkDoor "Birch" :: Door 'Opened
Door 'Opened

ghci> :t UnsafeMkDoor "Iron" :: Door 'Locked
Door 'Locked

ghci> :t UnsafeMkDoor @'Opened "Birch"
Door 'Opened

ghci> :t UnsafeMkDoor @'Locked "Iron"
Door 'Locked
```
