# Data.Foldable

```hs
-- All defined in Data.Foldable unless noted otherwise

type Foldable :: (* -> *) -> Constraint
class Foldable t where
    -- lazy map then fold of Monoids
    foldMap  :: Monoid m => (a -> m) -> t a -> m
    -- strict map then fold of Monoids
    foldMap' :: Monoid m => (a -> m) -> t a -> m
    -- Monoidal fold, e.g. Sum, Product, etc.
    fold     :: Monoid m => t m -> m

    -- lazy folds
    foldl    :: (b -> a -> b) -> b -> t a -> b
    foldr    :: (a -> b -> b) -> b -> t a -> b

    -- strict folds
    foldl'   :: (b -> a -> b) -> b -> t a -> b
    foldr'   :: (a -> b -> b) -> b -> t a -> b

    -- semi-folds (no default param)
    foldl1   :: (a -> a -> a) -> t a -> a
    foldr1   :: (a -> a -> a) -> t a -> a

    -- is empty?
    null     ::              t a -> Bool
    -- does the elem exists?
    elem     :: Eq a => a -> t a -> Bool

    length   :: t a -> Int
    toList   :: t a -> [a]

    minimum  :: Ord a => t a -> a
    maximum  :: Ord a => t a -> a

    sum      :: Num a => t a -> a
    product  :: Num a => t a -> a

{-# MINIMAL foldMap | foldr #-}


-- foldable structures

-- list
instance Foldable []

-- NonEmpty list
instance Foldable GHC.Base.NonEmpty

-- Data.Tree
instance Foldable Data.Tree.Tree

-- Maybe
instance Foldable Maybe

-- Either
instance Foldable (Either a)

-- Pair
instance Foldable ((,) a)

-- Array
instance Foldable (GHC.Arr.Array i)


-- Monoids: Sum, Product
instance Foldable Sum
instance Foldable Product

-- Monoids: First, Last
instance Foldable Data.Monoid.Last
instance Foldable Data.Monoid.First

-- Data.Semigroup
instance Foldable Data.Semigroup.Last
instance Foldable Data.Semigroup.First

instance Foldable Data.Semigroup.Min
instance Foldable Data.Semigroup.Max
instance Foldable Data.Semigroup.Option
instance Foldable (Data.Semigroup.Arg a)




-- Data.Functor

-- Defined in Data.Functor.Product
instance [safe] (Foldable f, Foldable g) 
  => Foldable (Data.Functor.Product.Product f g)

-- Defined in 'Data.Functor.Sum'
instance [safe] (Foldable f, Foldable g) 
  => Foldable (Data.Functor.Sum.Sum f g)

-- Defined in 'Data.Functor.Compose'
instance (Foldable f, Foldable g) 
  => Foldable (Data.Functor.Compose.Compose f g)

-- Defined in 'Data.Functor.Const'
instance Foldable (Data.Functor.Const.Const m)

-- Defined in 'Data.Functor.Identity'
instance Foldable Data.Functor.Identity.Identity



-- Defined in 'Control.Monad.Trans.Identity'
instance [safe] Foldable f 
  => Foldable (Control.Monad.Trans.Identity.IdentityT f)

-- Defined in 'Control.Applicative'
instance Foldable Control.Applicative.ZipList

-- Defined in 'Control.Monad.Trans.Error'
instance [safe] Foldable f => Foldable (Control.Monad.Trans.Error.ErrorT e f)


-- Defined in 'bifunctors-5.5.10:Data.Bifunctor.Biff'
instance [safe] forall k (p :: * -> * -> *) (g :: * -> *)
                (f :: k -> *) (a :: k).
  (Data.Bifoldable.Bifoldable p, Foldable g) 
  => Foldable (bifunctors-5.5.10:Data.Bifunctor.Biff.Biff p f g a)

-- Defined in 'bifunctors-5.5.10:Data.Bifunctor.Clown'
instance [safe] forall k (f :: k -> *) (a :: k).
  Foldable (bifunctors-5.5.10:Data.Bifunctor.Clown.Clown f a)

-- Defined in 'bifunctors-5.5.10:Data.Bifunctor.Tannen'
instance [safe] (Foldable f, Data.Bifoldable.Bifoldable p) =>
  Foldable (bifunctors-5.5.10:Data.Bifunctor.Tannen.Tannen f p a)

-- Defined in 'bifunctors-5.5.10:Data.Bifunctor.Joker'
instance [safe] forall k (g :: * -> *) (a :: k).
  Foldable g => Foldable (bifunctors-5.5.10:Data.Bifunctor.Joker.Joker g a)

-- Defined in 'profunctors-5.6.1:Data.Profunctor.Types'
instance Foldable (profunctors-5.6.1:Data.Profunctor.Types.Forget r a)



-- Defined in 'tagged-0.8.6.1:Data.Tagged'
instance [safe] forall k (s :: k).Foldable (tagged-0.8.6.1:Data.Tagged.Tagged s)

-- Defined in 'hashable-1.3.0.0:Data.Hashable.Class'
instance Foldable hashable-1.3.0.0:Data.Hashable.Class.Hashed



-- Defined in 'Data.Set.Internal'
instance Foldable Data.Set.Internal.Set
instance Foldable Data.HashSet.Internal.HashSet
instance Foldable Data.Sequence.Internal.ViewR
instance Foldable Data.Sequence.Internal.ViewL
instance Foldable Data.Sequence.Internal.Seq
instance Foldable Data.Sequence.Internal.Node
instance Foldable Data.Sequence.Internal.FingerTree
instance Foldable Data.Sequence.Internal.Elem
instance Foldable Data.Sequence.Internal.Digit


-- Defined in 'Data.Map.Internal'
instance Foldable Data.IntMap.Internal.IntMap
instance Foldable (Data.Map.Internal.Map k)
instance Foldable (Data.HashMap.Internal.HashMap k)

-- Defined in 'Data.Vector'
instance Foldable Data.Vector.Vector

-- Defined in 'Data.Complex'
instance Foldable Data.Complex.Complex


-- Generics
instance Foldable GHC.Generics.U1
instance Foldable GHC.Generics.V1
instance Foldable GHC.Generics.UAddr
instance Foldable GHC.Generics.UChar
instance Foldable GHC.Generics.UDouble
instance Foldable GHC.Generics.UFloat
instance Foldable GHC.Generics.UInt
instance Foldable GHC.Generics.UWord
instance Foldable GHC.Generics.Par1
instance Foldable Data.Proxy.Proxy
instance Foldable f => Foldable (GHC.Generics.Rec1 f)
instance Foldable f => Foldable (GHC.Generics.M1 i c f)
instance Foldable (GHC.Generics.K1 i c)
instance (Foldable f, Foldable g) => Foldable (f GHC.Generics.:.: g)
instance (Foldable f, Foldable g) => Foldable (f GHC.Generics.:+: g)
instance (Foldable f, Foldable g) => Foldable (f GHC.Generics.:*: g)


instance Foldable Data.Ord.Down
instance Foldable base-4.14.1.0:Data.Semigroup.Internal.Dual
instance Foldable f => Foldable (Data.Monoid.Ap f)
instance Foldable f => Foldable (base-4.14.1.0:Data.Semigroup.Internal.Alt f)



-- Defined in 'aeson-1.5.5.1:Data.Aeson.Types.Internal'
instance Foldable aeson-1.5.5.1:Data.Aeson.Types.Internal.Result
-- Defined in 'aeson-1.5.5.1:Data.Aeson.Types.Internal'
instance Foldable aeson-1.5.5.1:Data.Aeson.Types.Internal.IResult

-- Defined in 'http-client-0.7.5:Network.HTTP.Client.Types'
instance Foldable http-client-0.7.5:Network.HTTP.Client.Types.Response
-- Defined in 'http-client-0.7.5:Network.HTTP.Client'
instance Foldable http-client-0.7.5:Network.HTTP.Client.HistoriedResponse
```
