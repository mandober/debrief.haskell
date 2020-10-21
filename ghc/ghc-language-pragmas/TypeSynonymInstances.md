# FlexibleInstances

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances

Implies: `TypeSynonymInstances`

Allow definition of type class instances with arbitrary nested types in the instance head.

In Haskell 98 the head of an instance declaration must be of the form 
`C (T a1 ... an)`, where
- `C` is the class
- `T` is a data type constructor
- `a1 ... an` are distinct type variables

In the case of *multi-parameter type classes*, this rule applies to each parameter of the instance head.

GHC relaxes this rule in two ways:

1. With the `TypeSynonymInstances` extension, instance heads may use type synonyms. As always, using a type synonym is just shorthand for writing the RHS of the type synonym definition.

```hs
-- For example:
type Point a = (a,a)
instance C (Point a) where ...
-- is legal

-- The instance declaration is equivalent to
instance C (a,a) where ...

-- As always, type synonyms must be fully applied.
-- You cannot, for example, write:
instance Monad Point where ...
```

2. The `FlexibleInstances` extension allows the head of the instance declaration to mention arbitrary nested types.

For example, this becomes a legal instance declaration:
```hs
instance C (Maybe Int) where ...
```

See also the rules on overlap.

The FlexibleInstances extension implies TypeSynonymInstances.
