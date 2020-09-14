# Instance declaration

In its barest form, a type class just declares the functions that a type must define; this is achieved by stating functions' names and signatures.

An **instance declaration** is a language block in which we define functions specified by a type class; it is where we place the actual implementation of the prescribed methods.

```hs
{-# LANGUAGE InstanceSigs #-}

class Functor f where
  fmap :: (a -> b) -> f a -> fb

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b] -- (*)
  fmap = map
```

(*) Signatures are disallowed in instance blocks unless you enable the language pragma `{-# LANGUAGE InstanceSigs #-}`, after which you may write a prescribed method's signature, adjusted to the implementing instance. For example, the Functor class requires the function `fmap :: (a -> b) -> f a -> fb`; you can repeat this same signature in the instance declaration block or, more usefully, you can adjust it to the implementing type: `fmap :: (a -> b) -> [a] -> [b]` (fmap signature in terms of a list).







```hs
instance Eq Integer where
  x == y = x `integerEq` y
```

The function `integerEq` is a primitive function that compares integers for equality, but in general any valid expression is allowed on the right-hand side, just as for any other function definition.

The declaration is essentially saying: "The type `Integer` is hereby declared an instance of the `Eq` class by defining the `==` function in the following manner".

```hs
instance Eq Float where
  (==) = floatEq
```

Similarly, we have add another type, Float, to the class by also defining its behaviour wrt `==`.

Let's examine Eq class:

```hs
class Eq a where
  (==), (/=)            :: a -> a -> Bool
  x /= y                =  not (x == y)
```

The type sigs are given for both operations (they're the same), but `/=` also has a method body defined - this is *default implementation*.

If a method for a particular operation is omitted in an instance declaration, then the default one defined in the class declaration (if any) is used.
