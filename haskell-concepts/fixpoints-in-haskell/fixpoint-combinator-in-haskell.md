# Fixed point combinator in Haskell

The function `fix` from Data.Function is defined as (1), although it may be equivalently defined by (2). The definition (3) is also valid but it's not defined with an anonymous function like the previous two.

```hs
fix :: (a -> a) -> a
fix f = x where x = f x   -- (1)
fix f = let x = f x in x  -- (2)
fix f = f (fix f)         -- (3)
```

The fixpoint function `fix` takes some function `f` as an argument and return its fixpoint value `x` such that `f x = x`.


## Analytical and logical fixpoints

From an application perspective, there are many kinds of fixed points, but we can focus on these two approaches to finding fixpoints of functions: analytical and logical, and the distinction between *logical fixed points* and *analytical fixed points*.

A logical fix point can be written quite beautifully in Haskell as

```hs
fix :: (a -> a) -> a
fix f = x where x = f x
-- or even
fix f = f (fix f)
```

This *logical* `fix` ends up being a certain kind of natural way to discuss and introduce recursion into a language.

The *analytical* `fix` shows up often in numerical computing and has a somewhat different, but related, meaning. We'll begin with the type.

```hs
fixa :: (a -> a -> Bool) -> (a -> a) -> a -> Int -> a
```

This is clearly more complex than the type of `fix` was, as it represents a *guarded descent*.

Let's begin to write `fixa` by binding the args:

```hs
fixa :: (a -> a -> Bool) -> (a -> a) -> a -> Int -> a
fixa p iter z n = …
```

The goal is to repeatedly apply the `iter` function arg to the initial point `z` until either `n` (a positive integer) reaches 0, or the predicate `p` becomes True.

The implementation reads almost exactly as the prose:

```hs
fixa :: (a -> a -> Bool)    -- predicate
     -> (a -> a)            -- function to iterate
     -> a                   -- initial value
     -> Int                 -- upper bound for recursion
     -> a
fixa p iter z 0 = z
fixa p iter z n = loop z n
  where
  loop z n =
    let next = iter z
    in  if p next z
        then next
        else loop next (n - 1)
```


The value of a function like this is that we can use it to do iterative numerical algorithms, like Newton's Method

```hs
newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
newton f f' z =
  fixa (\a b -> a - b < 1e-6)  -- predicate
       (\x -> x - f x / f' x)  -- function to iterate
       z                       -- initial value
       1000                    -- upper bound for recursion
```


We can also improve it somewhat dramatically by using Haskell's lazy evaluation to spit out a lazy list of results instead of just the final point. When we do this we no longer need the manual loop counter as it's up to the consumer to decide how to manage this list of improvements.

```hs
fixaList :: (a -> a -> Bool) -> (a -> a) -> a -> [a]
fixaList p iter z = loop z
  where
  loop z = let next = iter z
           in  if (p next z)
               then cycle next      -- return `next` forever
               else z : loop next

fixa p iter z n = fixaList p iter z !! n
```


In fact, we don't need the `p` test anymore either, that can also be left to the consumer

```hs
fixaList :: (a -> a) -> a -> [a]
fixaList iter z = loop z where loop z = z : loop (iter z)

fixa iter z n = take n (fixaList iter z)
```


And now `fixaList` starts to look a bit like `fix`

```hs
fix      f      = x      where x      = f x
fixaList iter z = loop z where loop z = z : loop (iter z)
```


We can think of `fixaList` as specializing `fix`, thus we can use `fix` to write it:

```hs
fixaList iter z = fix (\loop z -> z : loop (iter z)) z
-- or, eta-reduced
fixaList iter   = fix (\loop z -> z : loop (iter z))
```


Which is a really long way of saying that:
> logical fixed points are strictly more powerful than analytical ones.
