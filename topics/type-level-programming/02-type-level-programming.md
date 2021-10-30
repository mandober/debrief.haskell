# Type-level programming

## Terms
- functions on types
- phantom types
- GADTs
- Proxy
- kinds
- promoted data ctors
- type families
- singletons
- recursive type classes
- functional dependencies

## Summary

- nullary type ctors are type-level constants
- any non-nullary type ctor is a type function
- `Maybe` type ctor is a binary type-level function, `Maybe :: Type -> Type`
- Maybe type is declared as `Maybe a = …`
- type variables, such as `a`, are camelCased
- types (type ctors) whether type functions or type args are PascalCased
- type operators have symbolic names, used infix
- symbolic name that begins with a colon (:) is considered camelCased✱
- ✱ on the RHS, i.e. as the name of data ctors
- `Type` is the kind of *inhabited types*
- all types (type functions) must eventually reduce to the kind `Type`
  - thus, any type function must return the type of the kind `Type`
  - any type's kind signature must end in `Type` (assuming inhabited types)
    - otherwise, it may end up in etc. like in the `Constraint` kind
  - all inhabited types classify term-level values
  - all inhabited types are saturated
  - saturated types (type ctors) have type params no more
  -   saturated and   inhabited types are common
  -   saturated and uninhabited types are possible (e.g. data promotion)
  - unsaturated and   inhabited types are not possible
  - unsaturated and uninhabited types are common
    - an unsaturated type is uninhabited
- uninhabited types are unsaturated type ctors, promoted data ctors, etc.
- `@Ͳ` is a syntax for appying a type function `Φ` to the type arg `Ͳ`

## Type functions

Nullary type ctors are type-level constants. Any non-nullary type ctor is a type function. For example, `Maybe` type ctor is a binary type-level function.

```hs
Maybe :: Type -> Type
Maybe @Int :: Type
Maybe @Char :: Type

Φ :: Type -> Type
Φ @Ͳ :: Type
```

The TypeApplication syntax, `@Ͳ`, applies a type function `Φ` to a type arg `Ͳ`.


## Phantom types

Phantom types provide a way to stash some extra info with a type.

```hs
-- | Wraps an underlying Integeral type `i` in a newtype annotated
-- with the bound `n`.
newtype i `Mod` (n :: Nat) = Mod i deriving (Eq, Ord)

-- | Extract the underlying integral value from a modular type.
--
-- >>> unMod (10 :: ℤ/4)  -- 2
unMod :: i `Mod` n -> i
unMod (Mod i) = i

-- | Synonym for `Mod`, ala ℤ/n syntax
type (/) = Mod

-- | Synonym for Integer, ala ℤ/n syntax
type ℤ = Integer

-- | The modulus has to be a non-zero type-level natural number.
type Modulus n = (KnownNat n, 1 <= n)

-- | Helper function to get the modulus out of ℤ/n as a value.
-- Used with type applications:
-- >>> modulus @5  -- 5
modulus :: forall n i. (Integral i, Modulus n) => i
modulus = fromInteger $ natVal (Proxy :: Proxy n)

-- | Injects a value of the underlying type into the modulus type,
-- wrapping as appropriate.
--
-- If `n` is ambiguous, specify it with TypeApplications (`n` must not be 0):
-- >>> toMod @6 10 -- 4
toMod :: forall n i. (Integral i, Modulus n) => i -> i `Mod` n
toMod i = Mod $ i `mod` (modulus @n)
```
