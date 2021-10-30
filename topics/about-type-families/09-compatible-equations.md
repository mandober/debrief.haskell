# Compatible equations

To say that overlapping equations are disallowed in open type families and allowed in closed type families would be an oversimplification. In practice, the rules are a bit more intricate.

Open type family instances must be compatible. Type family instances are compatible if at least one of the following holds:
* their LHSs are apart, i.e. not overlapping
* their LHSs unify with a substitution, under which the RHSs are equal

The second condition enables GHC to accept more programs. Consider the following example:

```hs
type family F a
type instance F a    = [a]
type instance F Char = String
```

While the LHSs clearly overlap (`a` is more general than `Char`), ultimately it makes no difference. If the user needs to reduce `F Char`, both equations will result in `[Char]`. The mathematically inclined readers will recognize this property as **confluence**.

Here's a more interesting example with several type variables:

```hs
type family G a b
type instance G a    Bool = a -> Bool
type instance G Char b    = Char -> b
```

The left-hand sides unify with a substitution (a ⟼ Char, b ⟼ Bool). The right-hand sides are equal under that substitution:

```hs
type instance G Char Bool = Char -> Bool
```

It is therefore safe to accept both of them: they are compatible.

Instance compatibility also plays a role in closed type families. Consider `FInteger` and `FString`:

```hs
type family FInteger a where
  FInteger Char = Integer
  FInteger a    = [a]

type family FString a where
  FString Char = String
  FString a    = [a]
```

Now, for an unknown `x`, could GHC reduce `FInteger x` to `[x]`? No, because the equations are matched top-to-bottom, and GHC first needs to check whether `x` is `Char`, in which case it would reduce to `Integer`.

On the other hand, the equations in `FString` are compatible. So if we have `FString x`, it doesn't matter whether `x` is `Char` or not, as both equations will reduce to `[x]`.
