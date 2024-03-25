# Haskell Debrief :: Syntax :: Aliases

Aliases
- aliasing a function
- aliasing a data type (type ctor)
  - aliasing a promoted data ctor (that has a tick, e.g. `'Zero`)
- aliasing a constraint
- aliasing a kind
- aliasing a class
- aliasing a type familiy


```hs
-- | aliasing functions

-- aliasing a function: eta-reduced
identity :: a -> a
identity = id

-- aliasing a function: eta-expanded
identity :: a -> a
identity x = id x


-- | aliasing data types

-- aliasing a saturated data type
type MyInt = Int

-- aliasing a data type: eta-reduced
type MyEither = Either
-- aliasing a data type: semi eta-expanded
type MyEither a = Either a
-- aliasing a data type: fully eta-expanded
type MyEither a b = Either a b
```


## Aliasing promoted data ctors

```hs
data Nat = Z | S Nat
```

If you want to use a data type, like `Nat`, only at the type level, i.e. only promoted via the `DataKinds` extension, without exporting its value-level definition, you can only export its two promoted data ctors (which are actually uninhabited type ctors) from the module. Their proper name includes the tick (i.e. `'Z` and `'S`), however you cannot export names prefixed with ticks (due to parsing limitations), and exporting their names without ticks refers to the data ctors (`Z` and `S`).




i.e. to only be used promoted the `DataKinds` extension,

A data type defined in the module that uses the `DataKinds` extension may be 


In the module that defines the data type, which is promoted using the `DataKinds` extension,


```hs
-- aliasing a promoted data ctor like 'Zero to remove the tick
type TyZero = 'Z
type TySucc = 'S
-- then export them using the 'type' qualifier:
module Nat (type TyZero, type TySucc ) where
```

### Module: ModNat

```hs
-- 2023-01-04
{-# LANGUAGE DataKinds #-}

module Nat.ModNat (type Nat, type TyZero, type TySucc) where
-- :load lib/Nat/ModNat.hs

-- ----------------------------------------------------------------------------
-- ModNat
-- ----------------------------------------------------------------------------
{-
  This module defines Nat to be promoted and used only at the type level,
  such that we have two uninhabited type ctors of kind Nat.

  We'd like to call these type-level nat ctors TyZero and TySucc
  and not 'Z and 'S

  Plus, we don't want the term-level definition at all,
  at least we don't want it exported!

  This module uses DataKinds extensions to promote the ctors, but the module that will import the type level naturals will not be using this extension.

  In the export list, we cannot write:
  - ('Z)
    Names with ticks cannot be contained in the export list:
    Parse error on input '''
  - (type 'Z)
    Names with ticks cannot be contained in the export list:
    Parse error on input '''
  - (Z)
    to mean the type ctor 'Z as this errors with:
    Not in scope: type constructor or class 'Z'
  - (type Z)
    to mean the type ctor 'Z as this errors with:
    Not in scope: type constructor or class 'Z'

  Note: data ctors canot be exported without also exporting the type they belong to (you can export the type without its ctors, though) e.g.
  - Nat(..) exports all data ctors
  - Nat(Z)  exports only the data ctors Z
  - Nat()   exports no data ctors (export smart ctors instead)
  - Nat     exports no data ctors (export smart ctors instead)



  The solution is to alias their asses, and than export only the aliases.
  In GHC 9.6 the new extension 'TypeData' will allow us to declare type-level only data declarations:

      type data Nat = Z | S Nat

  Until then, follow this procedure:

  0. Define term-level naturals with DataKinds ext enabled

      data Nat = Z | S Nat

  1.Make two type aliases:

      type TyZero = 'Z
      type TySucc = 'S

  2.Export only the two type aliases:

      module TyNat (     TyZero,      TySucc) where
      module TyNat (type TyZero, type TySucc) where

    The use of the 'type' qualifier is optional.

  3.The module that imports this module (TyNat) need not enable 'DataKinds'.
    If this module is imported qualified, e.g. as

      import TyNat qualified as N

    than these 2 names will be made available:

      N.TySucc    N.TyZero

    But the kind Nat will not be available:
    not as Nat, nor M.Nat, nor TyNat.Nat!

    We need to explicitly export the kind Nat using the 'type' qualifier:
      (type Nat)

    Such that the entire export list loks like:

      module Nat.ModNat (type Nat, type TyZero, type TySucc) where

    or, optinally also putting 'type' before the promoted ctors

      module Nat.ModNat (type Nat, type TyZero, type TySucc) where


    Then, importing this module will have 3 names available:

      N.TySucc    N.TyZero    N.Nat

    And we'll finally be able to also reference the kind Nat as N.Nat:

      >>> :k N.TyZero
      N.TyZero :: N.Nat

      >>> :k N.TySucc
      N.TySucc :: N.Nat -> N.Nat

      >>> :k N.Nat
      N.Nat :: Type


-}
data Nat = Z | S Nat

type TyZero = 'Z
type TySucc = 'S
```



### Module: ModNatImport

```hs
-- 2023-01-04
{-# LANGUAGE NoDataKinds #-}

module Nat.ModNatImport () where
-- :load lib/Nat/ModNatImport.hs

import Nat.ModNat qualified as N
import Data.Kind

-- ----------------------------------------------------------------------------
-- ModNatImport
-- ----------------------------------------------------------------------------
-- This module imports (only) the type-level naturals:
-- as the two ctors TySucc and TyZero of kind Nat

type Two = N.TySucc (N.TySucc N.TyZero)

type Vec :: forall k. k -> N.Nat -> Type
data Vec a (n :: N.Nat) where
```
