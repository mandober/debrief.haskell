# Injective type families

> Some type families are injective, i.e. we can deduce their inputs from their outputs.

For example, consider boolean negation:

```hs
type Not :: Bool -> Bool
type family Not x where
  Not True  = False
  Not False = True
```

If we know that `Not x` is `True`, then we can conclude that `x` is `False`. By default, the compiler does not apply such reasoning:

```hs
s :: forall x. (Not x ~ True, Typeable x) => String
s = show (typeRep @x)

ghci> s
<interactive>:7:1: error:
    • Couldn't match type 'Not x0' with ''True'
        arising from a use of 's'
      The type variable 'x0' is ambiguous
```

Even though the compiler can instantiate `x` to `False` based on the fact that `Not x` is `True`, it doesn't. Of course, we could do it manually, and GHC would check that we did it correctly:

```hs
ghci> s @False
"'False"

ghci> s @True
    <interactive>:12:1: error:
    • Couldn't match type ''False' with ''True'
        arising from a use of 's'
```

When we instantiate `x` to `False`, the `Not x ~ True` constraint is satisfied. When we attempt to instantiate it to `True`, the constrained is not satisfied and we see a type error.

There's only one valid way to instantiate `x`. Wouldn't it be great if GHC could do it automatically? *That's exactly what injective type families allow us to achieve*. Change the type family header of `Not` as follows:

```hs
type family Not x = r | r -> x where
  Not True  = False
  Not False = True
```

First, we give a name to the result of `Not x`, here `r`. Then, using the syntax of functional dependencies, we specify that `r` determines `x`. GHC will make use of this information whenever it needs to instantiate `x`:

```hs
ghci> s
"'False"
```

This feature is enabled by the `TypeFamilyDependencies` extension. As with ordinary functional dependencies, it is only used to guide type inference and cannot be used to produce equalities. So the following is, unfortunately, rejected:

```hs
not_lemma :: Not x :~: True -> x :~: False
not_lemma Refl = Refl
    -- Could not deduce: x ~ 'False
    -- from the context: 'True ~ Not x
```

That is a known limitation.
