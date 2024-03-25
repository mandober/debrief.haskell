# Type-level naturals

## Type-indexed data types

Besides the regular, parameterized data types, having the natural numbers at the type level would allows us to define **type-indexed data types**.

The natural numbers are ideal as an indexing type as they have an easily managable infinite sequence of type-level terms: `Z`, `S Z`, `S (S Z)`, …, **each of which *is* a distinct type**.

Moreover, they can be easily manupulated even at the type-level: we immediately have available the `S` type function for de/incrementing numbers, and we can define more functions on type-level naturals using type families.

### Vector

This opens the door to defining such data types as length-indexed lists, aka vectors, that unlike conventional lists, track their length in the type. This allows us to redefine partial functions on lists (e.g. `head`, `tail`) with the corresponding functions on vectors that are total. This is possible because we can distinguish an empty from a nonempty vector at compile time using nothing but their types. After compile time, this type information is erased - in fact, **type erasure** discards all type information following a successful compilation. The fact that types are not around at runtime is one one hand beneficial for efficiency, but on the other hand we may loose some important type-encoded information. For example, since it is precisely the length of the list that is lost at runtime, we have to define a function like `length` the old way - by counting each element - instead of merely reading of this info from the type. Until DependentHaskell, the only way we can retain such type-encoded information is using the singleton types.

## Defining type-level naturals: the old way

Before the modern GHC enhancements, the standard way to define type-level naturals was by declaring two uninhabited type ctors: `Z` to play the role of zero, and the parameterized `S`, as the successor function. The type ctor `Z` was acting as a term, ie. as the first natural number, zero. The type ctor `S`, parameterized by the type `a`, is to act as the successor (type-level) function allowing us to create more naturals.

While it works as long as one sticks to the indended use, the problem with this approach is that the type ctors `Z` and `S` are completely disjoint and unrelated. Even worse, the type param of `S` is unrestricted for instantiation at any type, even most don't make sense (in fact, only the types understood to be type-level naturals do); e.g. types like `S Bool` or `S (Int -> ())`. However, we have no principal way to constrain the type parameter `a` - we can only introduce some half-measures, and we cannot proclaim a clear invariant. And that goes against one of the core staples of FP: **make invalid states unrepresentable** (MISU) again.

```hs
-- | Zero
type Z :: Type
data Z

-- | Successor
type S :: Type -> Type
data S a
```

The `Z` and `S` type ctors have no data ctors, so they are uninhabited, but they are still useful for construction of type level naturals.

The two type ctors `Z` and `S` should be classified by the same kind, like the kind `Nat`, but we had no way to define new kinds.

Since `Z` and `S` are uninhabited types, they only introduce the naturals at the type-level, meaning we should choose another (term-level) numerical type when converting them into terms.

Converting a type-level entity into a term-level one is called **reification** (from Latin `res`, 'a thing', e.g. 'res publica'). We reify something abstract by making it representable. We can represent type-level naturals as the term level `Int`, but `Numeric.Natural` module has a more appropriate type. The type `Natural` is a virtually-infinite type; it is the unsigned version of the `Integer` type (while `Word` is unsigned version of `Int`).

* We can use a class to distinguish between the type-level naturals and to **convert type-level naturals to term-level naturals**.

```hs
import Numeric.Natural

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a

-- >>> reifyNat @(S (S Z))
```

`reifyNat` reifies a type-level natural into a term-level `Natural` value.

One way to think about `reifyNat` is as an **interpreter of a type-level language**. In this case, the type-level language is very simple, only capturing natural numbers, but in general, it could be arbitrarily complex and classes can be used to give it a meaning, even if the language has no meaningful term-level representation.



## Defining type-level naturals: the new way

Previously, the naturals were defined using two empty data declarations, `Z` and `S a`, which introduced these two (uninhabited) type ctors and naturals only at the type-level. We have reifyied them into the term-level naturals defined in the module `Numeric.Natural`.

If the type-level naturals are introduced using `DataKinds`, they can be reifyied in a similar way. 

```hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- This defines term-level naturals, which are auto-promoted to type level, giving us the 'Z and 'S (uninhabited) type ctors, bith of kind Nat.

data Nat = Z | S Nat


class Rei a where
  rei :: Nat

instance Rei Z where
  rei :: Nat
  rei = Z

instance Rei a => Rei (S a) where
  rei :: Nat
  rei = 1 + rei @a

x3,x4,x5 :: Nat
x3 = rei @Z          -- 0
x4 = rei @(S Z)      -- 1
x5 = rei @(S (S Z))  -- 2
```

## Reifying the type-level naturals: the wrong way

## DataKinds and data ctor promotion

## Peano-style naturals

We can define the naturals using the Peano-style unary representation:

```hs
-- as ADT
data Nat = Z | S Nat

-- as GADT

```


Using `DataKinds`, Peano-style naturals are automatically promoted, resulting in 
 two new but uninhabited type ctors 'Z and 'S classified under the same new kind 'Nat'.



  We'd like to have naturals at the type-level for type-indexing (e.g. vector), and we'd like to be able to reify them into term-level values.

  The aim is to define the Reify class that reifies the type-level nats into (for now) term-level naturals of the Numeric.Natural type (aka the unsigned Integer type). However, we'd like to be able to vary this target type of conversion, intstead of having it locked to Natural.

  THE FIRST ATTEMPT

  The first attempt, the Reify1 class:

    class Reify a where
      reify :: a -> Nat

  doesn't really reify shit as I came to realize non-immediately as I would have, had I slapped the type signature on it:

    type Reify :: Type -> Constraint
    class Reify a where
      reify :: a -> Nat

  The sig would make it clear that the method reify accepts an arg (term, value) of any INHABITED type, aka of the Type kind.


  all is does is accepts a term-level value of type `a`, converting it to a Nat. In fact, we're reconverting Nats to Nats, all at the term level. This is clear (also) from the signature of this class:

    type Reify :: Type -> Constraint

  meaning it accepts only inhabited Type
  not the uninhabited ones, and certainly not of the kind Nat.
  That's why it'd be better to make it into this:

    type Reify :: Nat -> Constraint

  restricting it to accept only the Nat kind of types.
  We must get rid of the type param `a` at the TERM level
  (not at the TYPE LEVEL, i.e. in the head of the class decl).
