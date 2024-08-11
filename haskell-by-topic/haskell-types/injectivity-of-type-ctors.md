# Type ctors

## Injectivity, generativity and matchability of type ctors

Haskell type ctors are injective and generative.

```hs
injective :: forall f a b. f a ~ f b => a -> b
injective = id

-- Maybe a ~ Maybe b => a ~ b


generative :: forall f g a b. f a ~ g b => f a -> g b
generative = id

-- f Int ~ g Int => f ~ g


matchability :: forall f g a b. (f a ~ g b) => (f a -> g b, a -> b)
matchability = (id, id)
```

The only way for `f a = g b` to hold is if `f = g` and `a = b`.

- injectivity:  `f` is injective iff           `f a = f b => a = b`
- generativity: `f` and `g` are generative iff `f a = g b => f = g`
- matchability: `f` is matchable iff `f` is both injective and generative. 
                `f` is matchable iff           `f a = g b => f = g ∧ a = b`


## Type families are not generally injective

```hs
data DBText
data User = User
data Pass = Pass

class DB a where
  type DBType a


instance DB User where
  type DBType User = DBText

instance DB Pass where
  type DBType Pass = DBText

-- instance DB UUID where
--   type DBType UUID = DBInt
-- instance DB Email where
--   type DBType Email = DBText
-- instance DB Date where
--   type DBType Date = DBDate

>>> :kind! DBType User :: Type
= DBText
>>> :kind! DBType Pass :: Type
= DBText
```

`DBType` has the same kind as the `Maybe` type ctor: `DBType :: Type -> Type`.

Haskell type ctors are all injective - if we try instantiating the `injective` function with `Maybe` and `Either`, GHC doesn't allow it if the type args `a` and `b` are not equal.

```hs
>>> :t injective @Maybe @Int @Int
injective @Maybe @Int @Int
  :: ((Maybe Int :: Type) ~ (Maybe Int :: Type)) => Int -> Int

>>> injective @Maybe @Int @Int 3
3
>>> injective @Maybe @Int @Char 3
• Couldn't match type Int with Char arising from a use of 'injective'


>>> :t injective @Either @Pass @User
injective @Either @Pass @User
  :: ((Either Pass :: (Type -> Type)) ~ (Either User :: (Type -> Type))) =>
     Pass -> User

-- this suggest that if
--   Either Pass = Either User
-- than
--   Pass = User
-- but if we saturate it:
>>> :t injective @Either @Pass @User Pass User
-- we get an error
• Couldn't match type Pass with User arising from a use of 'injective'
-- as we should because User and Pass are not equal
```


All type ctors in Haskell are injective:

    Maybe a = Maybe b => a = b

but type families are not injective in general:

    DBType a = DBType b ⇏ a = b

For example, `DBType User` is equal to `DBType Pass` (both are `DBText`), but we don't want that to mean that `User` is equal to `Pass`.

    DBType User = DBType Pass ⇏ User = Pass

Thus, `DBType` is neither injective nor generative.

```hs
>>> :t injective @Maybe
injective @Maybe 
  :: forall a b. ((Maybe a :: Type) ~ (Maybe b :: Type)) => a -> b

>>> :t injective @DBType
• The associated type family DBType should have 1 arg, but has been given none
```

GHC defends itself against the injectivity of type families by disallowing partial application of type families.
>Type families must be saturated.


## Unsaturated type families

We'd like to have type inference behave as it already does today, but also have the type level partial application.

The first idea, due to Richard Eisenberg, is about matchability of type ctors.

We somehow need to distinguish the kind of type ctors like `Maybe`, that are injective, generative and matchable, and the kind of ctors like `DBType` that are not. And we'd like to have it all be the first class citizens in the type system.

* Proposal: Unsaturated Type Families
https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html







## Refs

* Higher-order Type-level Programming in Haskell, Kiss Csongor, ICFP'19
https://www.youtube.com/watch?v=ZiGIBU0haOk&list=TLPQMDcwNDIwMjSMAbWhER01lg

* Unsaturated type families
https://stackoverflow.com/questions/49073150/type-family-returning-a-type

* Proposal: Unsaturated Type Families
https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html

* Kind level identity in Haskell
https://stackoverflow.com/questions/71277039/kind-level-identity-in-haskell
