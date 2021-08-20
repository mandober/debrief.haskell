# Advanced vectors

https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html

We'll analyse two approaches on how to design vectors.

The first, non-structural approach, considers a performant *fixed-length vector*, you would use for any code that requires a fixed-length container, especially for tight numeric code and situations where performance matters. We'll see how to implement them using the universal native `KnownNat` mechanisms, and also how we can implement them using *singletons* to help us make things a bit smoother and more well-integrated.

The second method is a *structural fixed-length inductive vector*. Actually, it is more like a *fixed-length (lazily linked) list* than a vector. The length of the list is enforced by the very structure of the data type. This type is more useful as a streaming type, and in situations where you want take advantage of the structural characteristics of lengths in the context of a dependently typed program.

## Non-structural approach

https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs

Noramally, vectors are used for their constant-time random access (indexing). Since Haskell already has a very optimized `vector` library, we can use it to get the main vector type defined there, and wrap it with a newtype that encodes the length using a phantom type parameter.

```hs
import Data.Vector qualified as V
import GHC.TypeNats

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
  deriving Show
```

`Vec n a` represents a `n`-element vector of `a`s, e.g. `Vec 5 Int`.

For numeric type, we use type literals, from the `GHC.TypeNats` module, that should be imported with *DataKinds* enabled. This extension promotes data ctors to type ctors, and type ctors to kinds. It makes available the kind `Nat`, and allows us to write arabic numerals as type-level numbers (type literals) which all have the kind `Nat`.

<!-- #region GHC.TypeNats -->

Available operations in `GHC.TypeNats` module
- arithmetic operators: `+, -, *, ^, Div, Mod, Log2`
- relational operators: `<=`, `<=?`, `CmpNat`

`CmpNat` type operation does a comparison and returns promoted `Ordering` type, so either `'LT`, `'EQ`, `'GT`.

`GHC.TypeNats.{Nat, KnownNat, natVal, natVal', sameNat, SomeNat, someNatVal}`

```hs
:kind 3
3 :: Nat

:kind! 3 + 6
3 + 6 :: Nat
= 9

:kind! 13 - 6
13 - 6 :: Nat
= 7

:kind! 2 ^ 3
2 ^ 3 :: Nat
= 8

:kind! 13 `Div` 6
13 `Div` 6 :: Nat
= 2

:kind! CmpNat 4 3
CmpNat 4 3 :: Ordering
= 'GT

:kind! 1 `CmpNat` 2
1 `CmpNat` 2 :: Ordering
= 'LT

:kind! (1 + 5) `CmpNat` (2 + 8)
(1 + 5) `CmpNat` (2 + 8) :: Ordering
= 'LT
```

<!-- #endregion --> 

> The process of *reflection* allows us to reflect a type-level number as a term-level number, with the help of the `KnownNat` class, that lets us obtain a number's runtime value using the `natVal` method.

```hs
natVal :: KnownNat n => p n -> Natural

-- 4real:
natVal :: KnownNat n => proxy n -> GHC.Num.Natural.Natural
```

where `Natural`, from `Numeric.Natural`, is a non-negative `Integer` type.

```hs
x1 = natVal (Proxy @10)          -- 10
x2 = natVal (Proxy :: Proxy 10)  -- 10
```

- `SomeNat` represents unknown type-level natural numbers
- `KnownNat` represents known type-level natural numbers


```hs
natVal :: forall n proxy. KnownNat n => proxy n -> Natural
natVal' :: forall n. KnownNat n => Proxy# n -> Natural
someNatVal :: Natural -> SomeNat
sameNat :: (KnownNat a, KnownNat b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
```

Low-level utilities for the `Nat` kind (like `natVal`) are found in the `GHC.TypeNats` module, and also in `GHC.TypeLits` for a slightly different API:
- http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html
- http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html


## Functions on type literals

```hs
type (<=) x y = (x <=? y) ~ 'True infix 4Source#

Comparison of type-level naturals, as a constraint.

Since: base-4.7.0.0

type family (m :: Nat) <=? (n :: Nat) :: Bool infix 4Source#

Comparison of type-level naturals, as a function. NOTE: The functionality for this function should be subsumed by CmpNat, so this might go away in the future. Please let us know, if you encounter discrepancies between the two.

type family (m :: Nat) + (n :: Nat) :: Nat infixl 6Source#

Addition of type-level naturals.

Since: base-4.7.0.0

type family (m :: Nat) * (n :: Nat) :: Nat infixl 7Source#

Multiplication of type-level naturals.

Since: base-4.7.0.0

type family (m :: Nat) ^ (n :: Nat) :: Nat infixr 8Source#

Exponentiation of type-level naturals.

Since: base-4.7.0.0

type family (m :: Nat) - (n :: Nat) :: Nat infixl 6Source#

Subtraction of type-level naturals.

Since: base-4.7.0.0

type family Div (m :: Nat) (n :: Nat) :: Nat infixl 7Source#

Division (round down) of natural numbers. Div x 0 is undefined (i.e., it cannot be reduced).

Since: base-4.11.0.0

type family Mod (m :: Nat) (n :: Nat) :: Nat infixl 7Source#

Modulus of natural numbers. Mod x 0 is undefined (i.e., it cannot be reduced).

Since: base-4.11.0.0

type family Log2 (m :: Nat) :: NatSource#

Log base 2 (round down) of natural numbers. Log 0 is undefined (i.e., it cannot be reduced).

Since: base-4.11.0.0

type family AppendSymbol (m :: Symbol) (n :: Symbol) :: SymbolSource#

Concatenation of type-level symbols.

Since: base-4.10.0.0

type family CmpNat (m :: Nat) (n :: Nat) :: OrderingSource#

Comparison of type-level naturals, as a function.

Since: base-4.7.0.0

type family CmpSymbol (m :: Symbol) (n :: Symbol) :: OrderingSource#

Comparison of type-level symbols, as a function.

Since: base-4.7.0.0

User-defined type errors
type family TypeError (a :: ErrorMessage) :: b where ...Source#

The type-level equivalent of error.

The polymorphic kind of this type allows it to be used in several settings. For instance, it can be used as a constraint, e.g. to provide a better error message for a non-existent instance,

-- in a context
instance TypeError (Text "Cannot Show functions." :$$:
                    Text "Perhaps there is a missing argument?")
      => Show (a -> b) where
    showsPrec = error "unreachable"
It can also be placed on the right-hand side of a type-level function to provide an error for an invalid case,

type family ByteSize x where
   ByteSize Word16   = 2
   ByteSize Word8    = 1
   ByteSize a        = TypeError (Text "The type " :<>: ShowType a :<>:
                                  Text " is not exportable.")
Since: base-4.9.0.0

data ErrorMessageSource#

A description of a custom type error.

Constructors

Text Symbol	
Show the text as is.

forall t. ShowType t	
Pretty print the type. ShowType :: k -> ErrorMessage

ErrorMessage :<>: ErrorMessage infixl 6	
Put two pieces of error message next to each other.

ErrorMessage :$$: ErrorMessage infixl 5	
Stack two pieces of error message on top of each other.
```
