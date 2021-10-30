# Non-parametric quantification

In terms, forall is a parametric quantifier, and this fact can be used to reason about functions. For example, consider the type signature of the identity function:

```hs
id :: forall a. a -> a
```

There's just one thing it can do with its argument: return it untouched. It could not, say, return 42 when given an integer:

```hs
id :: forall a. a -> a
id (x :: Int) = 42      -- Rejected!
id x = x
```

This is not only important for reasoning about code, but also to guarantee type erasure.

However, none of that applies to type families, which have their own interpretation of what forall is supposed to mean:

```hs
type F :: forall a. a -> a
type family F a where
  F (a :: Nat) = 42
  F a = a
```

This code is accepted and works without error:

```hs
ghci> :kind! F 0
F 0 :: Nat
= 42

ghci> :kind! F "Hello"
F "Hello" :: Symbol
= "Hello"
```

> On the one hand, this hinders our ability to reason about type families. On the other hand, this basically amounts to Π-types at the kind level, so it can be put to good use.
