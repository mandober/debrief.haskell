# Haskell by Topic :: Types in Haskell :: Type Equality

Equality
- (~)     lifted homogeneous equality
- (~~)    lifted heterogeneous equality
- (:~:)   kind homogeneous propositional equality
- (:~~:)  kind heterogeneous propositional equality
- (==)    Boolean equality on terms
- (->)    sorta like implication on terms and types (see return types below)


Type equality operator is defined as `~`. It is a constraint, so it can go in the type context.

```hs
injective :: forall f a b. (f a ~ f b) => a -> b
injective = id

generative :: forall f g a b. (f a ~ g b) => f a -> g b
generative = id

matchability :: forall f g a b. (f a ~ g b) => (f a -> g b, a -> b)
matchability = (id, id)
```

These operators are defined in `GHC.Types` module:

```hs
type (~) :: forall k. k -> k -> Constraint
class ((a :: k) ~ (b :: k)) => (~) @k a b
infix 4 ~

type (~~) :: forall k0 k1. k0 -> k1 -> Constraint
class ((a :: k0) ~~ (b :: k1)) => (~~) @k0 @k1 a b
infix 4 ~~
```

The operator `~` defines the **lifted homogeneous equality**. 
"Lifted" means it can give bogus results, e.g. due to deferred type errors. 
"Homogeneous" means two types `a` and `b` must have the same kinds to be equal.

The operator `~~` defines the **lifted heterogeneous equality**. 
"Lifted" means it can give bogus results, e.g. due to deferred type errors. 
"Heterogeneous" means two types `a` and `b` might have different kinds. 
Because `~~` can appear unexpectedly in error messages to users who do not care about the difference between heterogeneous equality (~~), and homogeneous equality (~), it is printed as `~` unless `-fprint-equality-relations` is set. 
In 0.7.0, the fixity was set to `infix 4` to match the fixity of `Data.Type.Equality.:~~:`.

```hs
(:~~:) :: forall k1 k2. k1 -> k2 -> Type
```

The `:~~:` is a type constructor defined in `Data.Type.Equality` module that defines **kind heterogeneous propositional equality**. Like `:~:`, `a :~~: b` is inhabited by a terminating value iff `a` is the same type as `b`.

```hs
type (:~:) :: forall {k}. k -> k -> Type
infix 4 :~:

-- Defined in Data.Type.Equality
type role (:~:) nominal nominal nominal
type (:~:) :: forall {k}. k -> k -> Type
data (:~:) @{k} a b where
  Data.Type.Equality.Refl :: forall {k} (a :: k). (:~:) @{k} a a

data a :~: b where
  Refl :: forall k (a :: k). a :~: a



-- INSTANCES:

-- All defined in 'Data.Type.Equality'
instance forall k (a :: k) (b :: k).
         ((a :: k) ~ (b :: k)) => Bounded ((:~:) @{k} a b)

instance forall k (a :: k) (b :: k).
         ((a :: k) ~ (b :: k)) => Enum ((:~:) @{k} a b)

instance forall k (a :: k) (b :: k).
         ((a :: k) ~ (b :: k)) => Read ((:~:) @{k} a b)

instance forall k (a :: k) (b :: k). Ord ((:~:) @{k} a b)

instance forall k (a :: k) (b :: k). Show ((:~:) @{k} a b)

instance forall k (a :: k) (b :: k). Eq ((:~:) @{k} a b)
```

Type ctor defined in `Data.Type.Equality` for **propositional equality**. 
If `a :~: b` is inhabited by some terminating value, then the type `a` is the same as the type `b`. 
To use this equality in practice, pattern-match on the `a :~: b` to get out the `Refl` constructor; in the body of the pattern-match, the compiler will know that `a ~ b`.



## Refs

* Kind level identity in Haskell
https://stackoverflow.com/questions/71277039/kind-level-identity-in-haskell
