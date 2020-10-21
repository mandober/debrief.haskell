# FlexibleContexts

Without FlexibleContexts all constraints must have type variables:
```hs
add :: Num a => a -> a
add = (+)
```
With FlexibleContexts enabled you can have any type inside a typeclass:
```hs
{-# LANGUAGE FlexibleContexts #-}
intAdd :: Num Int => Int -> Int
intAdd = (+)
```
FlexibleContexts usually goes along with `MultiParamTypeClasses`:

```hs
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

class Shower a b where
  myShow :: a -> b

method :: Shower a String => a -> String
method = myShow
```
Here you can see we say that we only want a `Shower a String`.
Without FlexibleContexts we'd have to replace String with a type variable.
