# Type equality

Type equality, or, more precisely, type unification is a type of constraint. It usually indicated that a type var unifies (is equal to) some other, often base, type, e.g. `a ~ Int`. Type equality operator `~` requires GADTs extension.

```hs
-- stupid example
funk :: (a ~ Int) => a -> a -> a
-- same as
funk :: Int -> Int -> Int

-- data ctors lifted to type ctors, type ctor to kinds with DataKinds
openStile :: (s ~ 'Opened) => Stile s -> Stile 'Opened
```

## Type inequality operator

https://chrisdone.com/posts/type-inequality-operator/

If you want to define a function that accepts all but a single type, you can make a type-level function using this (open) type family.

If the two types unify, we produce the type `'False` of kind `Bool`.

```hs
type family a /~ b where
  a /~ a = 'False
  _ /~ _ = 'True

-- usage
foo :: (Integral i, i /~ Word8 ~ 'True) => i -> ()
foo = undefined
```


The argument can be any type `i` that doesn't unify with `Word8`.

```hs
-- type-checks
bar = foo (undefined :: Int)

-- does not type-check
bad = foo (undefined :: Word8)
-- Could not match type 'False with 'True
```
