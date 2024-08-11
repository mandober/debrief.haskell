# Free variables

We need to get free vars of various items
- free evars of a term
- free tvars of a type
- free tvars of a type scheme
- free tvars of a typing context
So we define the class FreeVars with the method fv.

```hs
class FreeVars a where
  fv :: a -> Set String

instance FreeVars Exp where
  fv :: Exp -> Set String
  fv exp = case exp of
    EVar n     -> intoSet n
    EAbs v b   -> fv b ∖ intoSet v
    EApp m n   -> fv m ⋃ fv n
    ELit _     -> ø
    ELet x a b -> fv b ∖ intoSet x ⋃ fv a

instance FreeVars Typ where
  fv :: Typ -> Set String
  fv typ = case typ of
    TInt     -> ø
    TBool    -> ø
    TVar t   -> intoSet t
    TFun m n -> fv m ⋃ fv n

instance FreeVars Scheme where
  fv :: Scheme -> Set String
  fv (Scheme xs typ) = fv typ ∖ S.fromList xs
```
