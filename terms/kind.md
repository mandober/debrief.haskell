# Kinds

> Functions have arity while the type ctors have **kind**. 

Functions take value parameters (values as parameters) to produce values, while type constructors take type parameters (types as parameters) to produce types.

In Haskell, concrete types have kind `*`. For example, these types have kind `*` (aka `Type`): `Int`, `[Int]`, `Int -> String`, `Either Int Int`, `a`, `Maybe Int`, `Void`, etc. They are also called **saturated types**.

Both functions and type ctors, may be partially applied. Freshly defined, a function that expects 3 arguments is a *ternary* function (arity 3). Due to auto-currying and partial application, after this function receives just one argument, it returns (becomes) a *binary* function (arity 2), after another *unary*, and after the final, third, argument, it is fully saturated.

We can't check kind of function signatures (e.g. a -> a), only of type ctors, but we can go around this by making aliases.

```hs
type Unary a = a -> a
-- :k Unary :: * -> *

type Binary a b = a -> b
-- :k Binary :: * -> * -> *

type Ternary a b c = a -> b -> c
-- :k Ternary :: * -> * -> * -> *

-- Only declared type parameters determine kind:
type Ternary a b c = a -> b -> c -> a -> b -> c
-- :k Ternary :: * -> * -> * -> *

type Ternary a b c = (a -> b -> c) -> (a -> b) -> c
-- :k Ternary :: * -> * -> * -> *
```


Kind decreases as the type ctor gets partially applied:

```hs
:k Either            -- Either :: * -> * -> *
:k Either String     -- Either String :: * -> *
:k Either String Int -- Either String Int :: *
```
