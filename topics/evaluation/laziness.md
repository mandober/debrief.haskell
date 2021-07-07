# Laziness


An example of laziness int he context of pattern matching:

```hs
f :: Bool -> Bool -> Int
f _    False = 1
f True False = 2
f _    _     = 3
```

In a strict FPL with pattern matching, the second equation is redundant, but is it redundant in Haskell? No! Without it a call like `f undefined True` would return 3, but with it the same call diverges. This is because in order to see if the first arg matches the pattern `True` in the second equation, the first arg has to be evaluated, and since it is `undefined`, an attempt to evaluate it triggers an exception.

(the punchline) Moreover, the second equation is not redundant, even though this function cannot ever return 2 (!); the call `f True False` will return 1. The RHS of the second equation is inaccessible, which is recognized by GHC and mentioned in the *warning*: `[-Woverlapping patterns] pattern match has inaccessible right-hand side...`.

## Inhabitation and strict ctors

Another example with strict Maybe type:

```hs
data SMaybe a = SNothing | SJust !a

f :: SMaybe Void -> Int
f SNothing   = 1
f (SJust _ ) = 2  -- redundant

f SNothing        -- 1
f undefined       -- error triggered by the first equation (!)
```

The second equation is redundant. The only inhabitants of `SMaybe Void` are `SNothing` and bottom, and both are caught by the first equation.

`SJust undefined` doesn't exists, it is merely `undefined` since `SMaybe` is strict (call-by-value).

## Inhabitation and strict patterns

```hs
f :: Maybe Void -> Int
f Nothing    = 1
f (Just !_ ) = 2
```

Now, the pattern `_` is strict, it will match anything but it will evaluate it first.

`Void` has no inhabitants apart from `⟘`. `Maybe Void` has only 3 inhabitants: `⟘`, `Nothing` and `Just ⟘`.

The second equation is not redundant but it has inaccessible RHS.

## Pattern guards

```hs
last :: [a] -> Maybe a
last xs | (y:_) <- reverse xs = Just y
        | otherwise = Nothing

-- this pattern guard is the same as
last :: [a] -> Maybe a
last xs = case reverse xs of
  (y:_) -> Just y
  _     -> Nothing

-- VIEW PATTERNS
last :: [a] -> Maybe a
last (reverse -> y:_) = Just y
last (reverse -> [])  = Nothing

-- pattern guards can be mixed with normal pattern matching
get :: Maybe Int -> Int
get Nothing = 0
get x | Just y <- x = y
```
