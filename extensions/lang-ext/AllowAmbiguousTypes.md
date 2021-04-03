# AllowAmbiguousTypes

Lang Pragma: `AllowAmbiguousTypes`

We can use `Data.Typeable.typeRep` function to implement a function that will give us the name of the type. And we can do so without requiring the `Proxy` parameter:

```hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

typeName :: ∀a. Typeable a => String
--           ^           ^ a is ambiguous
typeName = show . typeRep $ Proxy @a

x1 = typeName @Bool           -- "Bool"
x2 = typeName @ (Maybe [Int]) -- "Maybe [Int]"
```

First, `Proxy @a` is written as shorthand for `Proxy :: Proxy a`. This is because the Proxy data constructor has type `Proxy t`. The type variable `t` here is the first one in its type signature, so we're capable of type applying it.

Second, type var `a` doesn't appear on the RHS of the context arrow, which is the only side Damas-Milner type inference works; it means the type parameter `a` in `typeName` can never be correctly inferred. Haskell refers to such a type as being **ambiguous** and, by default, won't compile such programs unless we enable the `AllowAmbiguousTypes` pragma. And actually using code with ambiguous types requires `TypeApplications` pragma.

These two extensions are thus either side of the same coin. AllowAmbiguousTypes allows us to define ambiguously typed functions and TypeApplications enables us to call them.

## Injectivity

Ambiguous types aren't always this obvious to spot. Consider this type family  and the type signatures - are they all unambiguous?

```hs
type family AlwaysUnit a where
  AlwaysUnit a = ()

f :: AlwaysUnit a -> a
g :: b -> AlwaysUnit a -> b
h :: Show a => AlwaysUnit a -> String
```

The `h` function has an ambiguous type var, `a`, because it's not clear which `Show a` instance we're asking for. Even though there is an `a` in `Show a => AlwaysUnit a -> String`, we are unable to access it. `AlwaysUnit a` is equal to `()` for all `a`'s!

More specifically, the issue is that `AlwaysUnit` doesn't have an inverse; there's no `Inverse` type family such that `Inverse (AlwaysUnit a)` equals `a`. In math, this lack of an inverse is known as non-injectivity. Because `AlwaysUnit` is non-injective, we're unable to learn what a is, given `AlwaysUnit a`.

The solution to non-injectivity is to give GHC some other way of determining the otherwise ambiguous type. This can be done like in our examples by adding a `Proxy a` parameter whose only purpose is to drive inference, or it can be accomplished by enabling `AllowAmbiguousTypes` at the definition site, and using `TypeApplications` at the call-site to fill in the ambiguous parameter
manually.
