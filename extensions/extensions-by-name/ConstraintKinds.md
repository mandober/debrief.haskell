# ConstraintKinds

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html#extension-ConstraintKinds

```hs
newtype Boo (c :: Type -> Constraint) a = Boo (a -> forall b. c b => b -> b)
newtype Boa a b = Boa (a -> Eq b => b -> b)

type Bar = Boo Eq Int
type Far = Boa Int Bool

boa :: (a -> Eq b => b -> b) -> Boa a b
boa = Boa

xx1 :: Far
xx1 = Boa \ (n :: Int) -> \ (b :: Bool) -> b
```
