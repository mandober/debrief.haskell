# Data.Kind

```hs
type Constraint :: *
data Constraint

type role FUN nominal representational representational

type FUN :: forall (n :: GHC.Types.Multiplicity) -> * -> * -> *
data FUN n a b

type GHC.Types.Type :: *
type GHC.Types.Type = TYPE 'GHC.Types.LiftedRep
```
