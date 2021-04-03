# Data.Typeable

```hs
:browse! Data.Typeable


-- imported via Data.Data, Data.Typeable
type TypeRep :: *
type TypeRep = base-4.14.1.0:Data.Typeable.Internal.SomeTypeRep

cast :: (Typeable a, Typeable b) => a -> Maybe b
eqT ::
  forall k (a :: k) (b :: k).
  (Typeable a, Typeable b) =>
  Maybe (a :~: b)
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep

gcast ::
  forall k (a :: k) (b :: k) (c :: k -> *).
  (Typeable a, Typeable b) =>
  c a -> Maybe (c b)
gcast1 ::
  forall k1 k2 (c :: k1 -> *) (t :: k2 -> k1) (t' :: k2 -> k1)
         (a :: k2).
  (Typeable t, Typeable t') =>
  c (t a) -> Maybe (c (t' a))
gcast2 ::
  forall k1 k2 k3 (c :: k1 -> *) (t :: k2 -> k3 -> k1)
         (t' :: k2 -> k3 -> k1) (a :: k2) (b :: k3).
  (Typeable t, Typeable t') =>
  c (t a b) -> Maybe (c (t' a b))

mkFunTy :: TypeRep -> TypeRep -> TypeRep
rnfTypeRep :: TypeRep -> ()
showsTypeRep :: TypeRep -> ShowS
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])

typeOf :: Typeable a => a -> TypeRep
typeOf1 :: Typeable t => t a -> TypeRep
typeOf2 :: Typeable t => t a b -> TypeRep
typeOf3 :: Typeable t => t a b c -> TypeRep
typeOf4 :: Typeable t => t a b c d -> TypeRep
typeOf5 :: Typeable t => t a b c d e -> TypeRep
typeOf6 :: Typeable t => t a b c d e f -> TypeRep
typeOf7 :: Typeable t => t a b c d e f g -> TypeRep

typeRep ::
  forall k (proxy :: k -> *) (a :: k).
  Typeable a =>
  proxy a -> TypeRep

typeRepArgs :: TypeRep -> [TypeRep]
typeRepFingerprint :: TypeRep -> GHC.Fingerprint.Type.Fingerprint
typeRepTyCon :: TypeRep -> TyCon


type role (:~:) nominal nominal
type (:~:) :: forall k. k -> k -> *
data (:~:) a b where
  ...


type role (:~~:) nominal nominal
type (:~~:) :: forall k1 k2. k1 -> k2 -> *
data (:~~:) a b where
  ...


HRefl :: forall k1 (a :: k1). a :~~: a
Proxy :: forall k (t :: k). Proxy t

type role Proxy phantom
type Proxy :: forall k. k -> *
data Proxy t = ...

Refl :: forall k (a :: k). a :~: a

type TyCon :: *
data TyCon = ...

type Typeable :: forall k. k -> Constraint
class Typeable a
  ...

rnfTyCon :: TyCon -> ()
tyConFingerprint :: TyCon -> GHC.Fingerprint.Type.Fingerprint
tyConModule :: TyCon -> String
tyConName :: TyCon -> String
tyConPackage :: TyCon -> String
```
