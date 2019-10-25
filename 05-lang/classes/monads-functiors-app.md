# Functors, Applicatives and Monads

http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html


## Context

There are simple, "naked", values, like `2`, but there are also values in a context, like `Just 2`. Context is like a metabox that wraps the values.

We say that "a value is in a context", or "a value with a context", or a "context value", for short.


```hs
-- x is a simple value:
x = 3

-- we can apply a simple function to a simple value:
succ 3 -- 4

-- cx is a value with a context (in a context):
cx = Just 3

-- we can't apply simple funcs to context values:
-- succ Just 3 -- ERROR

-- we need special funcs
```


When a (special) function is applied to a context value, the output result depends on the context; with different contexts we get different results. That is the main idea behind Functors, Applicatives, Monads, Arrows, etc.


For example, the `Maybe` type defines two related contexts for values: `Nothing` context demarks an absent value and `Just` context demarks a present one. Function application is different depending on which of these two context a value is in.

```hs
data Maybe a = Nothing | Just a
```

but first we must introduce...


## Functors

```hs
-- simple funcs can't work with context values, but
-- fmap is one of the funcs that can handle context values
fmap (+3) (Just 2) -- Just 5

-- its type has Functor constraint on data ctor f
fmap :: Functor f => (a -> b) -> f a -> f b
```

fmap is a HOF that takes a simple function, `(a -> b)`, and a context value, `f a` (`f` stands for Functor context); fmap returns a context value of (possibly) different type, `f b`.
