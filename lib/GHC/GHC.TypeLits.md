# GHC.TypeLits
https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-TypeLits.html

- module: `GHC.TypeLits`
- since: `base-4.6.0.0`

GHC's `DataKinds` language extension lifts data constructors, natural numbers, and strings to the type level. This module provides the primitives needed for working with type-level numbers, the `Nat` kind, and strings, the `Symbol` kind. It also defines the `TypeError` type family, a feature that makes use of type-level strings to support user defined type errors.

For now, this module is the API for working with type-level literals. However, please note that it is a work in progress and is subject to change. Once the design of the DataKinds feature is more stable, this will be considered only an internal GHC module, and the programmer interface for working with type-level data will be defined in a separate library.

The kinds `Nat` and `Symbol` are declared in `GHC.Types` in package `ghc-prim`. 
`KnownNat`, `KnownSymbol` are for linking type and value level.


GHC.TypeLits
* Kinds
  - Nat
  - Symbol
* Classes
  - KnownNat
    - natSing (gives the integer associated with a type-level nat)
  - KnownSymbol
    - symbolSing (gives the string associated with a type-level symbol)
* Data types
  - SomeNat (product)
  - SomeSymbol (product)
  - ErrorMessage (sum)
    - Text
    - ShowType
    - (:<>:)
    - (:$$:)
* Functions
  * on Nat
    - natVal
    - natVal'
    - sameNat
    - someNatVal
  * on Symbol
    - symbolVal
    - symbolVal'
    - sameSymbol
    - someSymbolVal
* Type families
  * on ErrorMessage
    - TypeError
  * on Symbol
    - AppendSymbol
    - CmpSymbol
  * on Nat
    - (+)
    - (-)
    - (*)
    - (^)
    - Div
    - Log2
    - Mod
    - (<=)
    - (<=?)
    - CmpNat


## Synopsis

```hs
data Nat
data Symbol

class KnownNat (n :: Nat)

natVal     :: forall n proxy. KnownNat n => proxy n -> Integer
natVal'    :: forall n. KnownNat n => Proxy# n -> Integer
someNatVal :: Integer -> Maybe SomeNat
sameNat    :: (KnownNat a, KnownNat b) 
           => proxy1 a -> proxy2 b -> Maybe (a :~: b)


class KnownSymbol (n :: Symbol)

symbolVal     :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal'    :: forall n. KnownSymbol n => Proxy# n -> String
someSymbolVal :: String -> SomeSymbol
sameSymbol    :: (KnownSymbol a, KnownSymbol b) 
              => proxy1 a -> proxy2 b -> Maybe (a :~: b)

data SomeNat = forall n.KnownNat n => SomeNat (Proxy n)
data SomeSymbol = forall n.KnownSymbol n => SomeSymbol (Proxy n)

type (<=) x y = (x <=? y) ~ 'True
type family (m :: Nat) <=? (n :: Nat) :: Bool

type family (m :: Nat) + (n :: Nat) :: Nat
type family (m :: Nat) * (n :: Nat) :: Nat
type family (m :: Nat) ^ (n :: Nat) :: Nat
type family (m :: Nat) - (n :: Nat) :: Nat

type family Div (m :: Nat) (n :: Nat) :: Nat
type family Mod (m :: Nat) (n :: Nat) :: Nat
type family Log2 (m :: Nat) :: Nat
type family CmpNat (m :: Nat) (n :: Nat) :: Ordering

type family AppendSymbol (m :: Symbol) (n :: Symbol) :: Symbol
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

type family TypeError (a :: ErrorMessage) :: b where ...

data ErrorMessage
  = Text Symbol
  | forall t. ShowType t
  | ErrorMessage :<>: ErrorMessage
  | ErrorMessage :$$: ErrorMessage
```


## More details

```hs
-- ----------------------------------------------------------------------------
-- Type families
-- ----------------------------------------------------------------------------

-- ====================
-- type families on Nat
-- ====================

type (+) :: Nat -> Nat -> Nat
type family (+) a b

type (-) :: Nat -> Nat -> Nat
type family (-) a b

type (*) :: Nat -> Nat -> Nat
type family (*) a b

type (^) :: Nat -> Nat -> Nat
type family (^) a b

-- | Division (round down) of natural numbers.
-- (Div x 0) is undefined, i.e. it cannot be reduced.
type Div :: Nat -> Nat -> Nat
type family Div a b

-- | Log base 2 (round down) of natural numbers.
-- Log 0 is undefined, i.e. it cannot be reduced.
type Log2 :: Nat -> Nat
type family Log2 a

-- | Modulus of naturals.
-- (Mod x 0) is undefined, i.e. it cannot be reduced.
type Mod :: Nat -> Nat -> Nat
type family Mod a b

-- | Comparison of type-level naturals, as a constraint.
type (<=) :: Nat -> Nat -> Constraint
type (<=) x y = (x <=? y) ~ 'True :: Constraint

-- | Comparison of type-level naturals, as a function.
-- NOTE: The functionality for this function should be subsumed by CmpNat, so this might go away in the future.
type (<=?) :: Nat -> Nat -> Bool
type family (<=?) a b

type CmpNat :: Nat -> Nat -> Ordering
type family CmpNat a b


-- =======================
-- type families on Symbol
-- =======================

-- | Concatenation of type-level symbols.
type AppendSymbol :: Symbol -> Symbol -> Symbol
type family AppendSymbol a b

-- | Comparison of type-level symbols, as a function.
type CmpSymbol :: Symbol -> Symbol -> Ordering
type family CmpSymbol a b


-- ==========================
-- type families on TypeError
-- ==========================

{-| The type-level equivalent of Prelude.error

The polymorphic kind of this type allows it to be used in several settings; it can be used as a constraint, e.g. to provide a better error message for a non-existent instance:

  -- (in some context)
  instance TypeError (Text "Cannot 'Show' functions." :$$:
                      Text "Perhaps there is a missing argument?")
      => Show (a -> b) where
    showsPrec = error "unreachable"


It can also be placed on the RHS of a type-level function to provide an error for an invalid case:

  type family ByteSize x where
    ByteSize Word16   = 2
    ByteSize Word8    = 1
    ByteSize a        = TypeError (Text "The type ":<>: ShowType a :<>:
                        Text " is not exportable.")
-}
type TypeError :: forall b. ErrorMessage -> b
type family TypeError a where


-- ----------------------------------------------------------------------------
-- Functions
-- ----------------------------------------------------------------------------
-- | Convert an integer into an unknown type-level natural.
someNatVal    :: Integer -> Maybe SomeNat

-- | We either get evidence that this function was instantiated
-- with the same type-level numbers, or Nothing.
sameNat :: (KnownNat a, KnownNat b)
  => proxy1 a
  -> proxy2 b
  -> Maybe (a Data.Type.Equality.:~: b)

natVal        :: KnownNat n => proxy n -> Integer
natVal'       :: KnownNat n => GHC.Prim.Proxy# n -> Integer



-- | Convert a string into an unknown type-level symbol.
someSymbolVal :: String -> SomeSymbol

-- | We either get evidence that this function was instantiated
-- with the same type-level symbols, or Nothing.
sameSymbol    :: (KnownSymbol a, KnownSymbol b)
              => proxy1 a
              -> proxy2 b
              -> Maybe (a Data.Type.Equality.:~: b)

symbolVal     :: KnownSymbol n => proxy n -> String
symbolVal'    :: KnownSymbol n => GHC.Prim.Proxy# n -> String

-- ----------------------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------------------
-- | Represents unknown type-level natural numbers.
type SomeNat :: *
data SomeNat = forall (n :: Nat). KnownNat n => SomeNat (Proxy n)
-- Data.Proxy.Proxy

-- | This type represents unknown type-level symbols.
type SomeSymbol :: *
data SomeSymbol = forall (n :: Symbol). KnownSymbol n => SomeSymbol (Proxy n)
-- Data.Proxy.Proxy

-- | A description of a custom type error.
type ErrorMessage :: *
data ErrorMessage
  = Text Symbol                     -- ^ Show the text as is.
  | forall t. ShowType t            -- ^ Pretty print the type.
  | ErrorMessage :<>: ErrorMessage  -- ^ Juxtapose two pieces of ErrorMessage
  | ErrorMessage :$$: ErrorMessage  -- ^ Stack two pieces of ErrorMessage


-- ----------------------------------------------------------------------------
-- Kinds
-- ----------------------------------------------------------------------------
type Nat :: *
data Nat

type Symbol :: *
data Symbol

-- ----------------------------------------------------------------------------
-- Classes
-- ----------------------------------------------------------------------------
type KnownNat :: Nat -> Constraint
class KnownNat n where
  GHC.TypeNats.natSing :: GHC.TypeNats.SNat n
  {-# MINIMAL natSing #-}

-- | This class gives the string associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
type KnownSymbol :: Symbol -> Constraint
class KnownSymbol n where
  GHC.TypeLits.symbolSing :: GHC.TypeLits.SSymbol n
  {-# MINIMAL symbolSing #-}


-- ----------------------------------------------------------------------------
-- PRIVATE
-- ----------------------------------------------------------------------------
newtype SSymbol (s :: Symbol) = SSymbol String

data WrapS a b = WrapS (KnownSymbol a => Proxy a -> b)

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSSymbol :: (KnownSymbol a => Proxy a -> b)
            -> SSymbol a      -> Proxy a -> b
withSSymbol f x y = magicDict (WrapS f) x y
```
