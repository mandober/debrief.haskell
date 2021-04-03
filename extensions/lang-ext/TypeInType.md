# TypeInType

The extension `TypeInType` is now deprecated: its sole effect is to switch on `PolyKinds` (which imply `KindSignatures`) and `DataKinds`.

Kind polymorphism:

```hs
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE KindSignatures       #-}
```
