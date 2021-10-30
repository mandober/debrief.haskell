# Type constructor arity

- higher-kinded types
- standalone kind signature
- saturated type constructors
- unsaturated type constructors
- non-generative type constructors
- generative type constructors




The arity of a type ctor is the number of type args it takes. 
It comes into play when we use *higher-kinded types*:

```hs
type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)
```

Now, what constitutes a valid arg to `S`? One might be tempted to think that any type constructor of kind `Type -> Type` could be used there, try a few:

```hs
MkS (Just True) Nothing           :: S Maybe
MkS (Left "Hi") (Right 42)        :: S (Either String)
MkS (Identity False) (Identity 0) :: S Identity
```

So `Maybe`, `Either String`, and `Identity` have all worked fine. But what about a type synonym?

```hs
type Pair :: Type -> Type
type Pair a = (a, a)
```

From the standalone kind signature, we see that it has the appropriate kind `Type -> Type`. GHCi also confirms this:

```hs
ghci> :kind Pair
Pair :: Type -> Type
```

And yet, any attempt to use `S Pair` is unsuccessful:

```hs
ghci> MkS (True, False) (0, 1) :: S Pair
<interactive>:6:29: error:
    • The type synonym 'Pair' should have 1 argument,
      but has been given none
```

Due to some limitations of the type system, **type synonyms cannot be partially applied**. The syntax of Haskell types has been chosen to ensure that, if two types have any unifying substitutions, then they have a most general unifier, which can be calculated by a simple variant of Robinson's algorithm (Robinson, 1965). One of the reasons for this is that there are no non-trivial equalities on types. Extending the type system with higher-order features (such as lambda expressions on types), or with other mechanisms that allow reductions or rewriting in the type language, could make unification undecidable, non-unitary (meaning that there may not be most general unifiers), or both. This, for example, is *why Haskell doesn't allow type synonyms to be partially applied* (and interpreted as some restricted kind of lambda expression).


In the case of `Pair`, we say that its arity is 1, as it needs one argument: `Pair Bool`, `Pair Integer`, and `Pair String` are all fine. On the other hand, `S Pair` or `Functor Pair` are not.

A type constructor whose arity requirements are met is called **saturated**, and **unsaturated** otherwise.

Note that we only need the notion of arity for type constructors that can reduce to other types when applied to an argument. For instance, `Pair Bool` is equal not only to itself but also to `(Bool, Bool)`

```hs
Pair Bool ~ Pair Bool     -- by reflexivity
Pair Bool ~ (Bool, Bool)  -- by reduction
```

On the other hand, `Maybe Bool` is only equal to itself:

```hs
Maybe Bool ~ Maybe Bool   -- by reflexivity
```

`Maybe` is a generative type ctor, while `Pair` is a non-generative.

> **Non-generative type constructors** have arities assigned to them and must be used saturated.

> **Generative type constructors** are not subject to such restrictions, so we do not apply the notion of arity to them.

Type family applications can also reduce to other types (are non-generative):

```hs
Append [1,2] [3,4] ~ Append [1,2] [3,4]  -- reflexivity
Append [1,2] [3,4] ~ [1, 2, 3, 4]        -- reduction
```

Therefore, **type families are non-generative** and have arities assigned to them. The arity is determined at definition site by taking into account the kind signature and the header:

```hs
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
```

The header contains `Append xs ys` rather than `Append xs` or simply `Append`. So, at first glance it may seem that the arity of `Append` is 2. However, we must also account for the `forall` bound variable `a`. In fact, even if you write `Append [1,2] [3,4]`, internally it becomes `Append @Nat [1,2] [3,4]`. Hence the arity of `Append` is 3.

The arity would still be 3 even if we didn't write out the `forall` explicitly:

```hs
type Append :: [a] -> [a] -> [a]
type family Append xs ys where

:kind! Append
Append :: [a] -> [a] -> [a]

:kind! Append @Nat
Append @Nat :: [Nat] -> [Nat] -> [Nat]

:kind! Append @Nat '[1,2]
Append @Nat '[1,2] :: [Nat] -> [Nat]
= Append '[1, 2]

:kind! Append @Nat '[1,2] '[3,4,5]
Append @Nat '[1,2] '[3,4,5] :: [Nat]
= '[1, 2, 3, 4, 5]

:kind! Append @_ '[1,2] '[3,4,5]
Append @_ '[1,2] '[3,4,5] :: [Nat]
= '[1, 2, 3, 4, 5]
```

But why is a header important? Couldn't we deduce the arity by counting the quantifiers in the kind signature? Well, that might work in most cases, but here is an interesting counter-example:

```hs
type MaybeIf :: Bool -> * -> *
type family MaybeIf b t where
  MaybeIf True  t = Maybe t
  MaybeIf False t = Identity t
```

This definition is assigned the arity of 2, and we can use it by applying it to two arguments:

```hs
data PlayerInfo b = MkPlayerInfo
  { name  :: MaybeIf b String
  , score :: MaybeIf b Integer
  }
```

This could be useful when working with a database. When reading a player record, we would expect all fields to be present, but a database update could touch only some of the fields:

```hs
dbReadPlayerInfo :: IO (PlayerInfo False)
dbUpdatePlayerInfo :: PlayerInfo True -> IO ()
```

In `PlayerInfo False` the fields are simply wrapped in Identity, e.g. `MkPlayerInfo { name = Identity "Jack", score = Identity 8 }`.

In `PlayerInfo True` the fields are wrapped in Maybe and therefore can be Nothing, e.g. `MkPlayerInfo { name = Nothing, score = Just 10 }`.

However, `MaybeIf` cannot be passed to `S`

```hs
ghci> newtype AuxInfo b = MkAuxInfo (S (MaybeIf b))
  • The type family 'MaybeIf' should have 2 arguments, but has been given 1
  • In the definition of data constructor 'MkAuxInfo'
    In the newtype declaration for 'AuxInfo'
```

Fortunately, this problem is solved by a minor adjustment to the definition of `MaybeIf` by removing the second type var `t`:

```hs
type MaybeIf :: Bool -> * -> *
type family MaybeIf b where
  MaybeIf True  = Maybe
  MaybeIf False = Identity
```

The kind signature remains unchanged, even though the `t` parameter is removed from the header and the clauses. With this tweak, the arity of `MaybeIf` becomes **1** and the definition of `AuxInfo` is accepted.

Exercise: determine the arity of `Not`, `FromMaybe`, and `Fst`.
