# Algebra

- https://bartoszmilewski.com/2017/02/28/f-algebras/
- https://www.youtube.com/watch?v=zkDVCQiveEo&list=PLMTONe7-tohmE02KadSPXvgpu26sn5j_n&index=35


## Monoid

A monoid is a set `m` with a pair of functions:
- μ :: m × m -> m (combines teo elements)
- η :: 1 -> m     (picks identity element)

A pair of functions is an element in the Cartesian product of two functions, 
`(μ, η) ∈ μ × η`, which are exponential objects `μ ∈ mᵐᵐ` and `η ∈ m`, so the Cartesian product of these two sets is `mᵐᵐ × m¹` = `mᵐᵐᐩ¹` = `m^(m² + 1)` ("+" is the coproduct in Set). We replaced a pair of functions with a single function which is an element of the set `m×m + 1 -> m`. Any element (function) of this set of functions is a potential monoid.

```hs
-- 1 + m×m -> m
data MonF a = MEmpty | MAppend a a

-- A MONOID ALGEBRA: multiplicative (⋀) monoid over 𝔹
alg :: MonF Bool -> Bool
alg  MEmpty       = True    -- additive identity
alg (MAppend m n) = m && n   -- additive binop, ⋀

-- A MONOID ALGEBRA: additive (+) monoid over ℤ
alg :: MonF Int -> Int
alg  MEmpty       = 0       -- additive identity
alg (MAppend m n) = m + n   -- additive binop

-- A MONOID ALGEBRA: multiplicative (*) monoid over ℝ
alg :: MonF Double -> Double
alg  MEmpty       = 0       -- multiplicative identity
alg (MAppend m n) = m * n   -- multiplicative binop

-- and so on, for other algebras like groups, rings over a carrier type

-- So, the type of an algebra is F a -> a
```

## Group

A group is a set `m` that adds the inverse operation to a monoid:
- η :: 1 -> m     (picks identity element)
- ι :: m -> m     (picks the inverse of an element)
- μ :: m × m -> m (combines two elements)

A 3-tuple of functions is an element in the Cartesian product of 3 functions, `(μ, ι, η) ∈ μ × ι × η`, which are exponential objects
- `μ ∈ mᵐᵐ`
- `ι ∈ mᵐ`
- `η ∈ m¹`

The Cartesian product of these 3 sets is: 
- `m¹  × mᵐ  × mᵐᵐ`  = `mᵐᵐᐩᵐᐩ¹`
- `m^1 × m^m × m^m²` = `m^(1 + m + m²)`

We replaced a triple of functions with a single function which is an element of:
- `1 + m + m×m -> m`

Any element (function) of this set of functions is a potential group.

```hs
-- 1 + m + m×m -> m === m⁰ + m¹ + m² -> m
data GrpF a = GId | GInv a | GOp a a

-- we can define A GROUP ALGEBRA: additive group over ℤ
alg :: GrpF Int -> Int
alg  GId      = 0       -- eval additive identity
alg (GInv m)  = - m     -- eval additive inverse
alg (GOp m n) = m + n   -- eval additive binop
```

## Ring

We can go on and define a ring by adding another binary operator (multipicative operation) and one nullary operator (multipicative identity), and so on.

`1 + m + m×m + 1 + m×m -> m` = 2 + m + 2m² -> m

```hs
-- 1 + m + m×m + 1 + m×m -> m  ===  2 + m + 2m² -> m
data RingF a
  = RingAddId                -- additive identity
  | RingAddInv a             -- additive inverse
  | RingAdd a a              -- additive binop
  | RingMulId                -- multiplicative identity
  | RingMul a a              -- multiplicative binop

-- we can define A RING ALGEBRA: additive+multiplicative ring over ℤ
alg :: RingF Int -> Int
alg  RingAddId     = 0       -- eval additive identity
alg (RingAddInv m) = - m     -- eval additive inverse
alg (RingAdd m n)  = m + n   -- eval additive binop
alg  RingMulId     = 1       -- eval multiplicative identity
alg (RingMul m n)  = m * n   -- eval multiplicative binop
```

## Sum of powers

We can vary the resulting algebra by picking different sets of operations and carrier types.

>Each algebra is descibed by a function type that is a sum of powers yielding the carrier type.

Each time we end up with a **function type that is a sum of powers** (on lhs), while the carrier set itself (on rhs):
- `a m⁰ + b m¹ + c m²  + … -> m` (where a, b, c … are constants ∈ ℕ)
- m⁰ + m¹ + m²  + … -> m
- 1  + m  + m²  + … -> m
- 1  + m  + m×m + … -> m

The sum of powers on the lhs usually also including the zeroth power, `1 = m⁰` (nullary exponential object as the identity element), which represents the terminal object. A unary exponential object, `m = m¹`, represents a unary operation like inverse. A binary exponential object, `m²`, represents a (e.g. addditive or multiplicative) binary operation; and so on for other arities. And in algebras that have two binary operations, the constants stand for their number, so e.g. a field algebra will have two terms `m²`, i.e. `2m²` for two binary operations, and `1` + `1` for two identities, and so on. In fact a field algebra is exactly `1 + 1 + m + m∙m + m∙m`, which is still a sum of powers.

## F-algebra

The sum of powers on the lhs of the arrow defines an endofunctor, as it is a functor from `m` back to `m`. If we pick an arbitrary endofunctor `F`, we wouldn't have to impose any constraints on the category and what we obtain is called an *F-algebra*.

An F-algebra is a triple of
- an object `a`
- an endofunctor `F`
- a morphism `F a -> a`

The object `a` is often called the carrier object or the underlying object, or, in the context of programming, the carrier type. The morphism is often called the *evaluation function* or the *structure map*. We can think of the functor `F` as forming expressions which are evaluated using the morphism.

Haskell's definition identifies an algebra with its evaluation function:

```hs
type Algebra f a = f a -> a
```

where
- `f` is a Functor (MonF, GroupF, RingF, etc.)
- `a` is a fixed carrier type (Int, Bool, etc.)


In the monoid algebra example, the functor was `MonF` which is `1 + a × a` (`1` for id and `a×a` for binop), and in the ring algebra it was `RingF` which is Haskell for `1 + 1 + a + a × a + a × a` (2 identities, 1 inverse op, 2 binops).

```hs
data MonF a = MEmpty | MAppend a a

-- A MONOID ALGEBRA: (⋀) monoid over 𝔹
alg :: MonF Bool -> Bool
alg  MEmpty       = True
alg (MAppend m n) = m && n
```

We see that the role of the functor (`MonF`) is to generate expressions that can be evaluated using the evaluator function, `alg`, of the algebra.


## Recursion

One way to generate arbitrary expression trees is to replace the applied occurrenced (in the ADT definition body) of the type param `a` with recursion.

First, we get rid of the binding occurrence of `a` in the ADT head, such that `MonF a` becomes `MonF`.

Second, we also delete the `-F` suffix because this type is not a functor anymore, so `MonF` becomes `Mon`.

Third, we obtain recursion by replacing all applied occurrenced of `a` in the ADT definition body with the name of the type, i.e. with `Mon` (such that `MAppend a a` becomes `MAppend Mon Mon`).

```hs
-- before
data MonF a = MEmpty | MAppend a a
-- the original evaluator...
alg :: MonF Int -> Int
alg  MEmpty       = 0
alg (MAppend m n) = m + n

-- after
data Mon    = MEmpty | MAppend Mon Mon
-- ...and its new recursive version
eval :: Mon -> Int
eval  MEmpty       = 0
eval (MAppend m n) = eval m + eval n

-- algebra is identified with the evaluation function
type Algebra f a = f a -> a
```

Note that we now have to wrap `m` and `n` with `eval` instead of just adding then up like before. Also note that all we can do here is addition with zeros, since the only literal we can form is 0. Sure, we can interpret `MEmpty` as 1 and then do addition with ones, which is better but not that great either.

However, we need to describe expression trees using the language of F-algebras, i.e. in terms of e.g. `MonF a`. We have to somehow formalize the process of replacing the free type variable `a` in the body of the ADT, recursively, with the result of the replacement.

```hs
-- original type on the rhs, replacement on the rhs
type RingF   a = RingF (RingF a)
-- this above cannot be typed, so we have

-- the depth-1 trees
type RingF₁  a = RingF (RingF a)

-- the depth-2 trees
type RingF₂  a = RingF (RingF (RingF a))
-- i.e.
type RingF₂  a = RingF (RingF₁ a)

-- the depth-n trees
type RingFₙ﹢₁ a = RingF (RingFₙ a)
```

Conceptually, after repeating this process infinitely many times, we end up with our expression `Exp` (with e.g. `Mon`).

Notice that `Exp` does not depend on `a`. The starting point doesn't matter, we always end up in the same place. This is not always true for an arbitrary endofunctor in an arbitrary category, but in the category `Set` it is.

## Fixpoint

Applying an endofunctor infinitely many times produces a fixpoint, an object defined as `Fix f = f (Fix f)`. The intuition is that, since we applied `f` infinitely many times to get `Fix f`, applying `f` once more doesn't change anything.

```hs
newtype Fix f = Fix { unFix :: (f (Fix f)) }

Fix :: f (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
```

The `unFix` is the accessor function that removes one level of functor application.

The accessor function `unFix` and `Fix` (data ctor) are each other's inverses:
- `Fix = unFix⁻¹`
- `unFix = Fix⁻¹`

## Category of F-algebras

Algebras over a given endofunctor `F` form a category:

>1) **OBJECTS**: algebras

An algebra, `𝒜𝓁ℊˣ ∈ Ob(𝒞)`, is a pair
- a carrier object `x`, where `x ∈ Ob(𝒞)`
- a structure map, `F x -> x`, where `F` is an endofunctor `F : 𝒞 -> 𝒞`

where both are from the original category 𝒞.

An algebra with carrier object `a ∈ Ob(𝒞)` and `F : 𝒞 -> 𝒞`    
  𝒜𝓁ℊᵃ = (a, F a -> a)

An algebra with carrier object `b ∈ Ob(𝒞)` and `F : 𝒞 -> 𝒞`    
  𝒜𝓁ℊᵇ = (b, F b -> b)


>2) **MORPHISMS**: ??? (what are the arrows in this category?)

To complete the definition, we have to define morphisms in the category of F-algebras. 

`(a, F a -> a) ⟼ (b, F b -> b) ∈ Ar(𝒞)`

A morphism of this category must map an algebra `(a,m)` to another `(b,n)`, where `f` and `g` are their structure maps, respectively:
- `m :: F a -> a`
- `n :: F b -> b`

To define morphisms, we first consider an arrow `f :: a -> b` that maps the carriers (the carrier objects of algebras) in the original category 𝒞. However, not just any ol' arrow will do because it must be compatible with both structure maps. Such structure-preserving arrows are called **homomorphisms**.

We can now define a homomorphism of F-algebras 
but first note that we can lift the map 
`f :: a -> b` to 
`F f :: F a -> F b` via the endofunctor `F`.
- way#1: starting at `F a`, we take `F f` followed by `n` to get to `b`.
- way#2: starting at `F a`, we take `m`   followed by `f` to get to `b`.

Therefore, the diagram below must commute as we need these paths to be equal:   

`f ∘ m = n ∘ F f`


```hs
F a          F f           F b
● ────────────────────────-> ●
│                            │
│m                           │n
│           f . m            │
│         = n . F f          │
│                            │
↓                            ↓
● ────────────────────────-> ●
a              f             b


-- structure map of algebra 1
m :: f a -> a

-- structure map of algebra 2
n :: f b -> b

-- mapping of the carriers
f :: a -> b

-- lifted arrow f
F f :: f a -> f b
F f = fmap f

-- these must be equal
f . m = n . fmap f
```

It's easy to verify that this is indeed a category: the identity morphisms from 𝒞 are still here, and the composition of homomorphisms is a homomorphism.

>An initial object in the category of F-algebras, if it exists, is called the initial algebra.

Let's call the carrier object of this initial algebra `i` and its evaluator function `j :: F i -> i`.

**Lambek's lemma** says that this evaluator function `j` is an isomorphism.

The proof of Lambek's lemma relies on the definition of the initial object, that is, on the fact that there is a unique homomorphism from the initial algebra (initial object) to any other F-algebra (object) in the category of F-algebras.

If we name this unique homomorphism `m`, the following diagram must commute:

```hs
F i          F m           F a
● ────────────────────────-> ●
│                            │
│j                           │f
│           m . j            │
│         = f . F m          │
│                            │
↓                            ↓
● ────────────────────────-> ●
i             m              a

-- unique homomorphism...
m :: i -> a

-- from the initial algebra...
j :: F i -> i

-- ...to any other algebra
f :: F a -> a

-- such that
m . j = f . fmap m
```


Now let's construct an algebra whose carrier is `F i`. The evaluator of such an algebra must be a morphism `F j :: F (F i) -> F i`. We can construct such an evaluator by lifting the evaluator `j` into `F j :: F (F i) -> F i`.

Because `(i, j :: F i -> i)` is the initial algebra, there must be a unique homomorphism `m` from `(i, j)` to `(F i, F j)`. The following diagram must commute:

```hs
F i          F m           F (F i)
● ────────────────────────-> ●
│                            │
│                            │
│j                           │F j
│                            │
│                            │
↓                            ↓
● ────────────────────────-> ●
i              m            F i
```

But we also have this trivially commuting diagram (both paths are the same!):

```hs
F (F i)      F j           F i
● ────────────────────────-> ●
│                            │
│                            │
│F j                         │j
│                            │
│                            │
↓                            ↓
● ────────────────────────-> ●
F i           j              i
```

which can be interpreted as showing that `j` is a homomorphism of algebras, mapping `(F i, F j)` to `(i, j)`.

We can glue these two diagrams together to get:

```hs
F i              F m →        F (F i)      ← F m                    F i
  ● ════════════════════════════ ● ═════════════════════════════════ ●
  ↑╷       ← F j                 ↑╷                 F j →           ↑╷
  ││                             ││                                 ││
 m││                          F m││                                m││
  ││                             ││                                 ││
  ││                             ││                                 ││
  ││                             ││                                 ││
  ││j                            ││F j                              ││j
  ││                             ││                                 ││
  ╵↓               m →           ╵↓         ← m                     ╵↓
  ● ════════════════════════════ ● ═════════════════════════════════ ●
  i          ← j                F i                   j →            i


j :: F i -> i
m :: i -> F i

F j :: F (F i) -> F i
F m :: F i -> F (F i)

1ᵢ = j . m
1ᵢ = j . F j . F m . m

m . j = F j . F m
```

This diagram may, in turn, be interpreted as showing that `j ∘ m` is a homomorphism of algebras. Only in this case the two algebras are the same.

Moreover, because `(i, j)` is the initial algebra, there can only be one homomorphism from it to itself, and that's the identity morphism, `1ᵢ`, which we know is a homomorphism of algebras.

Therefore `j ∘ m = 1ᵢ`. Using this fact and the commuting property of the left diagram we can prove that `m ∘ j = 1ꜰᵢ`.

This shows that `m` is the inverse of `j` and therefore `j` is an isomorphism between `F i` and `i`,

`F i ≅ i`

Which just says that `i` is a fixed point of the endofunctor `F`.

Back in Haskell, we recognize that `i` is `Fix f`, and `j` is the data ctor `Fix`, and `m` (`j`'s inverse) as `unFix`.

The isomorphism in Lambek's theorem tells us that, in order to get the initial algebra, we take the functor `f` and replace its argument `a` with `Fix f`.

We also see why the fixed point does not depend on `a`.

```hs
newtype Fix f = Fix (f (Fix f))

--     F  i      -> i
Fix :: f (Fix f) -> Fix f
-- j = m⁻¹ = Fix

--       i     -> F  i
unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f
-- m = j⁻¹ = unFix
```

So `F i ≅ i` are isomorphic, they are each other's inverses.


## Catamorphisms

https://wiki.haskell.org/Catamorphisms

```hs
type Algebra f a = f a -> a 

newtype Mu f = InF { outF :: f (Mu f) }

cata :: Functor f => Algebra f a -> Mu f -> a 
cata f = f . fmap (cata f) . outF

-- Alternate Definitions
cata f = hylo f outF
cata f = para (f . fmap fst)
```
