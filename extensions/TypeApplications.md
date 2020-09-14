# TypeApplications

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications

The `TypeApplications` GHC extension allows you to instantiate one or more of a polymorphic function's type arguments to a specific type. Use the `@` keyword to apply the function to a type.

The `TypeApplications` extension allows you to use visible type application in expressions. Here is an example: `show (read @Int "5")`. The `@Int` is the visible type application; it specifies the value of the type variable in read‘s type.

GHC tracks a distinction between what we call *inferred* and *specified* type variables. Only specified type variables are available for instantiation with visible type application.



Exploring in GHCi - type applications can be especially helpful in the REPL:

```hs
λ> :set -XTypeApplications

-- querying the type sig outputs polymorphic types
λ> :type (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- with this ext, you can request the type sig for a specific type
λ> :type (<*>) @[]
(<*>) @[] :: [a -> b] -> [a] -> [b]


-- polymorphic
λ> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- monomorphic
λ> :type (>>=) @Maybe
(>>=) @Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b


-- monomorphic
λ> :type traverse @Maybe @[]
traverse @Maybe @[] :: (a -> [b]) -> Maybe a -> [Maybe b]

-- monomorphic
:type traverse @[] @Maybe
traverse @[] @Maybe :: (a -> Maybe b) -> [a] -> Maybe [b]
```
