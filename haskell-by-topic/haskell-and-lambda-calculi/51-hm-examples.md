# HM examples

## Instantiation

- ∀a. a -> a >>> t0 -> t0
- ∀a. a -> a >>> int -> int

## Substituion

{ a ⟼ int, b ⟼ int}(a) = int
{ a ⟼ int, b ⟼ bool}(a → b) = bool
{ a ⟼ int, b ⟼ bool}(∀ a b. a → b) = ∀ a b. a → b
{ a ⟼ int, b ⟼ bool}(∀ a. a → b) = ∀ a. a → bool

## Unification

- inter-dependence: (a -> a) ⊔ (int -> b) = { a ⟼ int, b ⟼ int}
- occurs check:            a ⊔ (a -> b)   = occurs error 🧧
- self unification:        a ⊔ a          = {} = ∅



## Inference

### Example 1

```hs
e1 = \f -> const 1 (f true)
-- i.e.
e1 f = const 1 (f true)

-- (1)
e1 :: t1 -> t2
e1 = \f -> const 1 (f true)
-- (2)
e1 :: t1 -> t2
e1 = \(f :: t1) -> const 1 (f true)
-- (3)
const :: forall a b. a → b → a
const :: t3 → t4 → t3
const :: Int → t4 → Int
f true ~ t4
f :: t1
t1 ~ t5 → t6
f :: t5 → t6
t5 ~ Bool
f :: Bool → t6

e1 :: t1 -> t2
e1 :: (Bool → t6) -> Int

e1 :: forall a. (Bool → a) -> Int
```

1. exp `e1` is a lambda, so it gets a function type, `t1 → t2`
  - `t1` is the type of the formal param `f`, so `f : t1`
  - `t2` is the type of the e1's body

2. `f` is the formal param of `e1` so `f1 : t1`

2. `const` has a known type: `∀ a b. a → b → a`
  - which we instantiate as: `t3 → t4 → t3`
  - then `const` of type `t3 → t4 → t3` takes 2 args t3 and t4
  - t3 is known to be int, t3 ~ __int__
  - t4 will be the same type as `f true`, t4 ~ typeof(f true)
  - const ignores the 2nd arg and returns the 1st, which is int
  - so `const 1 _` has type int, typeof(const 1 _) = int as 1st arg is `int`
- literal 1 has a known type, `int`
  - it is the 1st arg to const, so the one being returned
  - meaning exp `const 1 whatever` has type int!
  - this tells us about f's body type - it must be `int`
  - so now, f : t1 → int
- `f true` is our lambda `t1 → t2` applied to `true` ie. a `bool`
  - so the input type of lambda must be `bool`, t1 ~ bool
  - `t1 → t2` ~ `bool → int`
  - thus, `f : bool → int`
- What is the arity of `f`?
  - it could be a unary fn that takes a bool and returns an int
    `f : bool → int`
    or maybe:
    `f : ∀a. (bool → a) → int`


If we had instad just `\f -> const 1`, we wouldn't be able to infer the type of `f` in terms of concrete types (that's alright), that is, this exp contains more info than that. Since we also have subexp `f true`, this subexp contributes info we'd like to preserve - and in order to do that we need to augment the return type of the `infer` function to the one that is a pair of `(Subst, Typ)`. Besides returning just the type (`Typ`), we need to also return these extra pieces of info we pick up while inferring, and for that we use the `Subst` datatype. The `Subst` datatype maps type variables (String) to types (Typ).

```hs
type Subst = Map String Typ

-- initially insufficient:
infer :: Ctx -> Exp -> TI Typ
-- updated to:
infer :: Ctx -> Exp -> TI (Subst, Typ)
```
