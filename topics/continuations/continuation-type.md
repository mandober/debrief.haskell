# Continuation type

## Continuations

A continuation has the type `(a -> r) -> r`, where `a` is the nominal return type, and `r` is the answer type. The `(a -> r)` function is called a continuation.

To see how this type comes about considering the identity function is enough. The type that represents functions in their most generic form, `(a -> b)`, cannot stand by itself (only as part of larger sugnature), so the next best type we can use as a showcase is the one for the `id` function, `(a -> a)`. It says that `id` takes an arg `a` and just returns it to the caller. Letting a function return its result directly to the caller is called **direct style**.

```hs
-- nominal fn
id :: a -> a
id x = x

-- the call
x :: Int
x = 5 -- 5
```

The alternative approach is called **continuation-passing style**, in which, instead of letting a function return the result to its caller, we pass it an extra function argument, called a continuation. Of course, a function's definition needs to be adjusted for the, by convention named `k`, continuation parameter. The function then applies `k` to the result it would otherwise return (the computation is continued thus).

```hs
-- cps fn
id :: a -> (a -> r) -> r
id x k = k x

-- call it
x = 5 id    -- 5 :: Int
x = 5 show  -- "5" :: String
x = 5 (:[]) -- [5] :: [Int]
```

**Answer Type Polymorphism**: the most important thing to note is that a cont influences the answer type, which is no longer limited to a single type. That is, each call can have a different type. To remain so flexible, the signature needs to keep the type variables (and not replace them with concrete types), e.g. `a -> (a -> Float) -> Float`, while still a cont, ceases to be polymorphic in the answer type.

```hs
Int -> (Int -> Float) -> Float
a   -> (a   -> Float) -> Float
Int -> (Int -> r)     -> r
a   -> (a   -> r)     -> r
```

Some terminology:
- *nominal return type* (that is, value) is the type (value) a function would normally return to the caller, `a`; *nominal result*.
- function arg (farg) is a continuation, `(a -> r)`
- *answer type* is a function's overall return type, influenced by a cont, `r`
- a function declares an extra param, `k`, to bind the cont
- *current continuation*: there is only one continuation at any moment
- a function applies `k` to its nominal result


## Continuation data type

Haskell makes continuations a standalone data type (at least so they can be made instances of various classes).

```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
```
