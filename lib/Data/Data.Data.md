# Data.Data

```hs
> :browse! Data.Data

-- imported via Data.Data
NoRep      :: DataRep
IntRep     :: DataRep
FloatRep   :: DataRep
CharRep    :: DataRep
IntConstr   :: Integer  -> ConstrRep
FloatConstr :: Rational -> ConstrRep
CharConstr  :: Char     -> ConstrRep

AlgConstr   :: ConIndex -> ConstrRep
AlgRep     :: [Constr] -> DataRep

Infix  :: Fixity
Prefix :: Fixity

type ConIndex :: *
type ConIndex = Int

type Constr :: *
data Constr = ...ETC

type ConstrRep :: *
data ConstrRep = ...ETC

type Data :: * -> Constraint

class Typeable a => Data a
  ...ETC

type DataRep :: *
data DataRep = ...ETC

type DataType :: *
data DataType = ...ETC

type Fixity :: *
data Fixity = ...ETC

constrRep    :: Constr -> ConstrRep
constrFields :: Constr -> [String]
constrFixity :: Constr -> Fixity
constrIndex  :: Constr -> ConIndex
constrType   :: Constr -> DataType

dataCast1 ::
  (Data a, Typeable t) =>
  (forall d. Data d => c (t d)) -> Maybe (c a)

dataCast2 ::
  (Data a, Typeable t) =>
  (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c a)

dataTypeOf      :: Data a => a -> DataType
dataTypeName    :: DataType -> String
dataTypeRep     :: DataType -> DataRep
dataTypeConstrs :: DataType -> [Constr]

fromConstr  :: Data a => Constr -> a
fromConstrB :: Data a => (forall d. Data d => d) -> Constr -> a
fromConstrM :: (Monad m, Data a) => (forall d. Data d => m d) -> Constr -> m a


gfoldl ::
  Data a =>
  (forall d b. Data d => c (d -> b) -> d -> c b)
  -> (forall g. g -> c g) -> a -> c a

gmapM :: (Data a, Monad m) => (forall d. Data d => d -> m d) -> a -> m a

gmapMo ::
  (Data a, GHC.Base.MonadPlus m) =>
  (forall d. Data d => d -> m d) -> a -> m a

gmapMp ::
  (Data a, GHC.Base.MonadPlus m) =>
  (forall d. Data d => d -> m d) -> a -> m a

gmapQ :: Data a => (forall d. Data d => d -> u) -> a -> [u]

gmapQi :: Data a => Int -> (forall d. Data d => d -> u) -> a -> u

gmapQl ::
  Data a =>
  (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r

gmapQr ::
  Data a =>
  (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r

gmapT :: Data a => (forall b. Data b => b -> b) -> a -> a

gunfold ::
  Data a =>
  (forall b r. Data b => c (b -> r) -> c r)
  -> (forall r. r -> c r) -> Constr -> c a

indexConstr :: DataType -> ConIndex -> Constr

isAlgType   :: DataType -> Bool
isNorepType :: DataType -> Bool

maxConstrIndex :: DataType -> ConIndex

mkCharType :: String -> DataType
mkCharConstr :: DataType -> Char -> Constr
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkDataType :: String -> [Constr] -> DataType
mkFloatType :: String -> DataType
mkIntType :: String -> DataType
mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkNoRepType :: String -> DataType
mkRealConstr :: (Real a, Show a) => DataType -> a -> Constr

readConstr :: DataType -> String -> Maybe Constr
repConstr :: DataType -> ConstrRep -> Constr
showConstr :: Constr -> String
toConstr :: Data a => a -> Constr

tyconModule :: String -> String
tyconUQname :: String -> String


-- imported via Data.Data, Data.Typeable
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
type TypeRep :: *
type TypeRep = base-4.14.1.0:Data.Typeable.Internal.SomeTypeRep
type Typeable :: forall k. k -> Constraint

class Typeable a
  ...

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
rnfTyCon :: TyCon -> ()
rnfTypeRep :: TypeRep -> ()
showsTypeRep :: TypeRep -> ShowS
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])

tyConFingerprint :: TyCon -> GHC.Fingerprint.Type.Fingerprint
tyConPackage :: TyCon -> String
tyConModule :: TyCon -> String
tyConName :: TyCon -> String

typeOf  :: Typeable a => a -> TypeRep
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
typeRepTyCon :: TypeRep -> TyCon
typeRepFingerprint :: TypeRep -> GHC.Fingerprint.Type.Fingerprint
```
