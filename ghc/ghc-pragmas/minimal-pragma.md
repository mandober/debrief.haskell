# MINIMAL pragma

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html#minimal-pragma

- Pragma's allowed location: class body
- Specify which methods are requred for a minimal complete instance of a class.

The `MINIMAL` pragma is used to specify the minimal complete definition of a class, i.e. it prescribes the methods that are required for a complete class defnition. If an instance doesn't satisfy the prescribed minimal complete definition, then a warning is generated.

The pragma is useful when methods have defaults defined in terms of each other, i.e. *circular defaults*. For example,

```hs
class Eq a where

    {-# MINIMAL (==) | (/=) #-}

    (==) :: a -> a -> Bool
    (==) x y = not (x /= y)

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)

instance Eq Bool          -- (1)

instance Eq Bool where    -- (2)
  (==) True  True  = True
  (==) False False = True
  (==) _ _         = False
```

Without the `MINIMAL` pragma NO WARNING would be generated when a type just declares it being an instance of the class, without actually implementing either method, as in (1). This warning can be turned off with the flag `-Wno-missing-methods`.

A type that does define at least one of the prescribed methods, gets the other for free, as in (2), although it may provide a more efficient definition for it as well.

If a class declaration doesn't specify the MINIMAL pragma, it is just as if a pragma that lists all methods `{-# MINIMAL op₁, op₂, ..., opₙ #-}` was given, where the `opᵢ` are the methods that lack a default method in the class declaration.

The syntax for minimal complete definition is:

```js bnf
mindef := method_name
        |  '(' mindef ')'
        |  mindef ',' mindef
        |  mindef '|' mindef
```

* A vertical bar denotes disjunction, i.e. one of the two sides is required.
* A comma denotes conjunction, i.e. both sides are required.
* Conjunction binds stronger than disjunction.
