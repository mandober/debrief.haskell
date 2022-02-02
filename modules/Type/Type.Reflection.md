# Type

```hs
import Type.Reflection

moduleName :: Module -> String
modulePackage :: Module -> String
rnfModule :: Module -> ()
rnfSomeTypeRep :: SomeTypeRep -> ()
rnfTyCon :: TyCon -> ()
rnfTypeRep :: forall {k} (a :: k). TypeRep @k a -> ()
someTypeRepTyCon :: SomeTypeRep -> TyCon
splitApps :: forall {k} (a :: k). TypeRep @k a -> (TyCon, [SomeTypeRep])
tyConModule :: TyCon -> String
tyConName :: TyCon -> String
tyConPackage :: TyCon -> String
typeOf :: forall a. Typeable @Type a => a -> TypeRep @Type a
typeRep :: forall {k} (a :: k). Typeable @k a => TypeRep @k a
typeRepKind :: forall k (a :: k). TypeRep @k a -> TypeRep @Type k
typeRepTyCon :: forall {k} (a :: k). TypeRep @k a -> TyCon
withTypeable :: forall k (a :: k) r. TypeRep @k a -> (Typeable @k a => r) -> r

someTypeRep ::
  forall {k} (proxy :: k -> Type) (a :: k).
  Typeable @k a =>
  proxy a -> SomeTypeRep

eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
  TypeRep @k1 a -> TypeRep @k2 b -> Maybe ((:~~:) @k1 @k2 a b)





--(they do have 2 distinct contexts, forgot why)
pattern App
  :: forall k2 (t :: k2)
  .  ()                                     -- given context (?)
  => forall k1 (a :: k1 -> k2) (b :: k1)
  .  ((t :: k2) ~ (a b :: k2))              -- to derive context (?)
  => TypeRep @(k1 -> k2) a
  -> TypeRep @k1 b
  -> TypeRep @k2 t

pattern Con
  :: forall k (a :: k).
     () =>
     ((base-4.16.0.0:Data.Typeable.Internal.IsApplication
         @k a :: GHC.Types.Symbol)
      ~ ("" :: GHC.Types.Symbol)) =>
     TyCon -> TypeRep @k a

pattern Con'
  :: forall k (a :: k).
     () =>
     ((base-4.16.0.0:Data.Typeable.Internal.IsApplication
         @k a :: GHC.Types.Symbol)
      ~ ("" :: GHC.Types.Symbol)) =>
     TyCon -> [SomeTypeRep] -> TypeRep @k a

pattern Fun
  :: forall k (fun :: k).
     () =>
     forall (r1 :: GHC.Types.RuntimeRep) (r2 :: GHC.Types.RuntimeRep)
            (arg :: TYPE r1) (res :: TYPE r2).
     ((k :: Type) ~ (Type :: Type),
      (fun :: k) ~~ (arg -> res :: Type)) =>
     TypeRep @(TYPE r1) arg -> TypeRep @(TYPE r2) res -> TypeRep @k fun
HRefl :: forall {k1} (a :: k1). (:~~:) @k1 @k1 a a



type Module :: Type
data Module = GHC.Types.Module GHC.Types.TrName GHC.Types.TrName
-- Defined in 'GHC.Types'
instance Eq Module
-- Defined in 'GHC.Classes'
instance Show Module
-- Defined in 'GHC.Show'


type SomeTypeRep :: Type
data SomeTypeRep where
  SomeTypeRep :: forall k (a :: k). !(TypeRep @k a) -> SomeTypeRep
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance Eq SomeTypeRep
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance Ord SomeTypeRep
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance Show SomeTypeRep
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'


type TyCon :: Type
data TyCon = GHC.Types.TyCon GHC.Prim.Word#
             GHC.Prim.Word#
             Module
             GHC.Types.TrName
             GHC.Prim.Int#
             GHC.Types.KindRep
-- Defined in 'GHC.Types'
instance Eq TyCon   -- Defined in 'GHC.Classes'
instance Ord TyCon  -- Defined in 'GHC.Classes'
instance Show TyCon -- Defined in 'GHC.Show'


type Typeable :: forall k. k -> Constraint
class Typeable @k a

-- type role TypeRep nominal nominal
-- type TypeRep :: forall k. k -> Type
-- data TypeRep @k a where







-- kind-insensitive type equality
type role (:~:) nominal nominal nominal
type (:~:) :: forall {k}. k -> k -> Type
data (:~:) @{k} a b where
  Refl :: forall {k} (a :: k). (:~:) @{k} a a
-- Defined in 'Data.Type.Equality'
infix 4 :~:

--       forall k (a :: k) (b :: k). Eq (a :~: b)
instance forall k (a :: k) (b :: k). Eq   ((:~:) @{k} a b)
instance forall k (a :: k) (b :: k). Ord  ((:~:) @{k} a b)
instance forall k (a :: k) (b :: k). Show ((:~:) @{k} a b)

instance forall k (a :: k)   (b :: k).
                 ((a :: k) ~ (b :: k))
               => Enum (a :~: b)
            -- => Enum ((:~:) @{k} a b)

instance forall k (a :: k)   (b :: k).
                 ((a :: k) ~ (b :: k)) =>
                Bounded ((:~:) @{k} a b)

instance forall k (a :: k)   (b :: k).
                 ((a :: k) ~ (b :: k)) =>
                   Read ((:~:) @{k} a b)


-- kind-sensitive type equality

type role (:~~:) nominal nominal nominal nominal
type (:~~:) :: forall k1 k2. k1 -> k2 -> Type
data (:~~:) @k1 @k2 a b where
  HRefl :: forall {k1} (a :: k1). (:~~:) @k1 @k1 a a
-- Defined in 'Data.Type.Equality'
infix 4 :~~:

instance forall k1 k2 (a :: k1) (b :: k2). Eq   ((:~~:) @k1 @k2 a b)
instance forall k1 k2 (a :: k1) (b :: k2). Ord  ((:~~:) @k1 @k2 a b)
instance forall k1 k2 (a :: k1) (b :: k2). Show ((:~~:) @k1 @k2 a b)

instance forall k1 k2 (a :: k1) (b :: k2).
         ((a :: k1) ~~ (b :: k2)) =>
         Enum ((:~~:) @k1 @k2 a b)

instance forall k1 k2 (a :: k1) (b :: k2).
         ((a :: k1) ~~ (b :: k2)) =>
         Bounded ((:~~:) @k1 @k2 a b)

instance forall k1 k2 (a :: k1) (b :: k2).
         ((a :: k1) ~~ (b :: k2)) =>
         Read ((:~~:) @k1 @k2 a b)




type role TypeRep nominal nominal
type TypeRep :: forall k. k -> Type
data TypeRep @k a where
  TrType  :: TypeRep @Type Type
  TrTyCon :: forall k (a :: k).
    { trTyConFingerprint :: Fingerprint
    , trTyCon            :: !TyCon
    , trKindVars         :: [SomeTypeRep]
    , trTyConKind        :: !(TypeRep @Type k)
    }
    -> TypeRep @k a


{-
type role TypeRep nominal nominal
type TypeRep :: forall k. k -> Type
data TypeRep @k a where
  base-4.16.0.0:Data.Typeable.Internal.TrType  :: TypeRep @Type Type
  base-4.16.0.0:Data.Typeable.Internal.TrTyCon :: forall k (a :: k).
  { base-4.16.0.0:Data.Typeable.Internal.trTyConFingerprint
      :: {-# UNPACK #-}GHC.Fingerprint.Type.Fingerprint
  , base-4.16.0.0:Data.Typeable.Internal.trTyCon     :: !TyCon
  , base-4.16.0.0:Data.Typeable.Internal.trKindVars  :: [SomeTypeRep]
  , base-4.16.0.0:Data.Typeable.Internal.trTyConKind :: !(TypeRep @Type k)
  }
  -> TypeRep @k a
-}


base-4.16.0.0:Data.Typeable.Internal.TrApp :: forall k1 k
  (a1 :: k1 -> k) (b :: k1).
  { base-4.16.0.0:Data.Typeable.Internal.trAppFingerprint
    :: {-# UNPACK #-}GHC.Fingerprint.Type.Fingerprint
  , base-4.16.0.0:Data.Typeable.Internal.trAppFun 
    :: !(TypeRep @(k1 -> k) a1)
  , base-4.16.0.0:Data.Typeable.Internal.trAppArg :: !(TypeRep @k1 b)
  , base-4.16.0.0:Data.Typeable.Internal.trAppKind :: !(TypeRep @Type k)
  }
  -> TypeRep @k (a1 b)

base-4.16.0.0:Data.Typeable.Internal.TrFun :: forall (m :: GHC.Types.Multiplicity)
(r1 :: GHC.Types.RuntimeRep)
(r2 :: GHC.Types.RuntimeRep) (a1 :: TYPE r1)
(b :: TYPE r2).
{base-4.16.0.0:Data.Typeable.Internal.trFunFingerprint :: {-# UNPACK #-}GHC.Fingerprint.Type.Fingerprint,
base-4.16.0.0:Data.Typeable.Internal.trFunMul :: !(TypeRep
@GHC.Types.Multiplicity
m),
base-4.16.0.0:Data.Typeable.Internal.trFunArg :: !(TypeRep
@(TYPE
r1)
a1),
base-4.16.0.0:Data.Typeable.Internal.trFunRes :: !(TypeRep
@(TYPE
r2)
b)}
-> TypeRep @Type (a1 %m -> b)
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance forall k (a :: k). Eq (TypeRep @k a)
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance forall k (a :: k). Ord (TypeRep @k a)
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
instance forall k (a :: k). Show (TypeRep @k a)
-- Defined in 'base-4.16.0.0:Data.Typeable.Internal'
```
