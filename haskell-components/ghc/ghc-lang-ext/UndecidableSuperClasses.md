# Undecidable superclass

`UndecidableSuperClasses` 
Allow all superclass constraints, including those that may result in
non-termination of the typechecker. Unlocks much more flexible constraints in superclasses.

A class cannot be declared recursively; generally, it cannot have itself as a superclass.

```hs
-- illegal:
class C a => D a where ...
class D a => C a where ...
```

GHC implements this test conservatively when type functions, or type variables, are involved. For example:

```hs
type family F a :: Constraint
-- or
class F a => C a where ...
```

GHC will complain about this, because you might later add

```hs
type instance F Int = C Int
```

ending up in a superclass loop.

Here's an example involving a type variable:

```hs
class f (C f) => C f
class c       => Id c
```

If we expanded the superclasses of `C Id` we'd get first `Id (C Id)` and thence `C Id` again.


However, superclass constraints like these are sometimes useful, and the conservative check is annoying where no actual recursion is involved.

Moreover, genuinely-recursive superclasses are sometimes useful. Here's a real-life example:

```hs
class
  ( Frac (Frac a) ~ Frac a
  , Fractional (Frac a)
  , IntegralDomain (Frac a)
  )
  => IntegralDomain a
  where
  type Frac a :: Type
```

Here the superclass cycle does terminate but it's not entirely straightforward to see that it does.

With the language extension `UndecidableSuperClasses` GHC lifts all restrictions on superclass constraints. If there really *is* a loop, GHC will only expand it to finite depth.
