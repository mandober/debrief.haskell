# Substitution

Substitutions are represented as mappings from tvars (`TName` = `String`) to types, `Typ`, i.e. `type Subst = Map TName Typ`. The `Subst` type is just an alais for `Map TName Typ`.


- `type Subst = Map EName Typ`  (evars)
- `type Subst = Map TName Typ`  (tvars)
- `type Subst = Map String Typ` (both)

A substitution may be applied to a
- type
- type scheme
- typing context

which warrents creating a class to have the method name `apply` or `substitute` commonly available.

Substitution are composed, but that only warrents a single function like `composeSubst`.

A substitution only replaces free tvars (type variables) - the quantified type variables in a scheme (in the `Typ` part) are not affected by a substitution

    {subst-map}(scheme) = type

    {a → int, b → bool}(∀b.a → b) = ∀b.int → b
    {a → b, b → a}((a → b) → b → a) = (b → a) → a → b

```hs
type Subst = Map TName Typ

class Substitute a where
  apply :: Subst -> a -> a
```

```hs
type Subst  = Map     TName  Typ
data Scheme = Scheme [TName] Typ
```

Subst:

    s1 :: Map TName Typ
    s1 = [ "a" ⟼ int, "c" ⟼ bool ]

Scheme:

    scheme1 :: Scheme [TName] Typ
    scheme1 = Scheme ["a", "b"] (a → b → c :: Typ)

First remove from `s1` all tvars that are in the list of qtvars, producing the changed Subst `s2`:

    s2 =
    = s1 ∖ (qtvars of scheme1)
    = [ "a" ⟼ int, "c" ⟼ bool ] ∖ ["a", "b"]
    = [ "c" ⟼ bool ]

Thus, `s2 = [ "c" ⟼ bool ]` and it may be now applied to the `scheme1`, producing the type scheme `scheme2 :: Scheme`

    scheme2 =
    = applySubst s2 scheme1
    = applySubst @Typ
        [ "c" ⟼ bool ]
        ["a", "b"] (a → b → c :: Typ)
    = Scheme ["a", "b"] (a → b → bool)


```hs
scheme2 :: Subst -> Scheme -> Scheme
scheme2 = applySubst @Scheme s1 scheme1
        = applySubst @Typ    s2 scheme1
  where
  s2 = applySubst @Typ s2 scheme1
     = Scheme ["a", "b"]
        $ applySubst @Typ
          [ "c" ⟼ bool ]
          (a → b → c :: Typ)
     = Scheme ["a", "b"]
          (a → b → bool :: Typ)
```

## Composing substitutions

```hs
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.union s2' s1
  where s2' = M.map (applySubst s1) s2
-- i.e.
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.union (M.map (applySubst s1) s2) s1
```

>The order of combining substitutions matters:
`s1 ∘ s2` favores the **right substitution** when entries have the same key.


```hs
-- Composing substitutions
s1 =      [ "t0" ⟼ TInt ]
s2 =      [ "t0" ⟼ TBool ]
s1 ∘ s2 = [ "t0" ⟼ TBool ] -- favores s2
s2 ∘ s1 = [ "t0" ⟼ TInt ]  -- favores s1

-- Composing substitutions
s1 =      [ "t0" ⟼ TBool, "t1" ⟼ TInt                         ]
s2 =      [                "t1" ⟼ TBool,     "t2" ⟼ TVar "t9" ]
s1 ∘ s2 = [ "t0" ⟼ TBool, "t2" ⟼ TVar "t9", "t1" ⟼ TBool     ]
s2 ∘ s1 = [ "t0" ⟼ TBool, "t2" ⟼ TVar "t9", "t1" ⟼ TInt      ]
```
