# ScopedTypeVariables

Lang Pragma: `ScopedTypeVariables`

Type vars are scoped throughout the function when this pragma is enabled.

```hs
{-# LANGUAGE ScopedTypeVariables #-}

working :: forall a b. (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a
```

The nested function `apply` refers to the same `b` from the `working` function.

> Scoped-Type-Variables gives us the means to reference a type variable outside of the contexts in which it was declared. However, this behavior is only turned on for types that begin with an **explicit forall quantifier**.
