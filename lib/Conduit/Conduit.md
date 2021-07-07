# Conduit (shorter)

```hs
($$)    :: Monad m => Source m a -> Sink a m b -> m b
($$+)   :: Monad m => Source m a -> Sink a m b -> m (SealedConduitT () a m (), b)

($=)    :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
(=$)    :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
(=$=)   :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r

(=$$+)  :: Monad m => ConduitT a b m () -> ConduitT b Void m r -> ConduitT a Void m (SealedConduitT a b m (), r)

(.|)    :: Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r

($$++)  :: Monad m => SealedConduitT () a m () -> Sink a m b -> m (SealedConduitT () a m (), b)
($$+-)  :: Monad m => SealedConduitT () a m () -> Sink a m b -> m b
($=+)   :: Monad m => SealedConduitT () a m () -> Conduit a m b -> SealedConduitT () b m ()
(=$$++) :: Monad m => SealedConduitT  i o m () -> ConduitT o Void m r -> ConduitT i Void m (SealedConduitT i o m (), r)
(=$$+-) :: Monad m => SealedConduitT  i o m () -> ConduitT o Void m r -> ConduitT i Void m r


type Acquire :: * -> *
newtype Acquire a =
  resourcet-1.2.4.2:Data.Acquire.Internal.Acquire (
    (forall b. IO b -> IO b)
    -> IO (resourcet-1.2.4.2:Data.Acquire.Internal.Allocated a)
  )

type BufferAllocStrategy :: *
type BufferAllocStrategy =
  (IO Data.Conduit.Combinators.Buffer, Int
   -> Data.Conduit.Combinators.Buffer
   -> IO (IO Data.Conduit.Combinators.Buffer)
  )

type Conduit :: * -> (* -> *) -> * -> *
type Conduit i m o = ConduitT i o m ()

type ConduitM :: * -> * -> (* -> *) -> * -> *
type ConduitM = ConduitT :: * -> * -> (* -> *) -> * -> *

type role ConduitT nominal nominal nominal representational
type ConduitT :: * -> * -> (* -> *) -> * -> *
newtype ConduitT i o m r
  = conduit-1.3.4.1:Data.Conduit.Internal.Conduit.ConduitT
  { conduit-1.3.4.1:Data.Conduit.Internal.Conduit.unConduitT ::
    forall b. 
      (r -> conduit-1.3.4.1:Data.Conduit.Internal.Pipe.Pipe i i o () m b)
         -> conduit-1.3.4.1:Data.Conduit.Internal.Pipe.Pipe i i o () m b
  }

type Consumer :: * -> (* -> *) -> * -> *
type Consumer i m r = forall o. ConduitT i o m r

type Flush :: * -> *
data Flush a = Chunk a | Flush

type Identity :: * -> *
newtype Identity a = Identity {runIdentity :: a}



type MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
  {-# MINIMAL liftIO #-}


type MonadResource :: (* -> *) -> Constraint
class MonadIO m => MonadResource m where
  resourcet-1.2.4.2:Control.Monad.Trans.Resource.Internal.liftResourceT
  :: ResourceT IO a -> m a
  {-# MINIMAL liftResourceT #-}


type MonadThrow :: (* -> *) -> Constraint
class Monad m => MonadThrow m where
  throwM :: GHC.Exception.Type.Exception e => e -> m a
  {-# MINIMAL throwM #-}

type MonadTrans :: ((* -> *) -> * -> *) -> Constraint
class MonadTrans t where
  lift :: Monad m => m a -> t m a
  {-# MINIMAL lift #-}


type MonadUnliftIO :: (* -> *) -> Constraint
class MonadIO m => MonadUnliftIO m where
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b
  {-# MINIMAL withRunInIO #-}


type PrimMonad :: (* -> *) -> Constraint
class Monad m => PrimMonad m where
  type PrimState :: (* -> *) -> *
  type family PrimState m
  primitive :: (ghc-prim-0.7.0:GHC.Prim.State# (PrimState m)
                -> (# ghc-prim-0.7.0:GHC.Prim.State# (PrimState m), a #))
               -> m a
  {-# MINIMAL primitive #-}


type Producer :: (* -> *) -> * -> *
type Producer m o = forall i. ConduitT i o m ()

type ReleaseType :: *
data ReleaseType = ReleaseEarly | ReleaseNormal | ReleaseException


type role ResourceT representational nominal
type ResourceT :: (* -> *) -> * -> *
newtype ResourceT m a
  = resourcet-1.2.4.2:Control.Monad.Trans.Resource.Internal.ResourceT
  {resourcet-1.2.4.2:Control.Monad.Trans.Resource.Internal.unResourceT
  :: GHC.IORef.IORef resourcet-1.2.4.2:Control.Monad.Trans.Resource.Internal.ReleaseMap
  -> m a}


type role SealedConduitT nominal nominal nominal nominal
type SealedConduitT :: * -> * -> (* -> *) -> * -> *
newtype SealedConduitT i o m r
  = conduit-1.3.4.1:Data.Conduit.Internal.Conduit.SealedConduitT (conduit-1.3.4.1:Data.Conduit.Internal.Pipe.Pipe
    i i o () m r)


type Sink :: * -> (* -> *) -> * -> *
type Sink i = ConduitT i Void :: (* -> *) -> * -> *

type Source :: (* -> *) -> * -> *
type Source m o = ConduitT () o m ()

type Void :: *
data Void


type role ZipConduit nominal nominal nominal representational
type ZipConduit :: * -> * -> (* -> *) -> * -> *
newtype ZipConduit i o m r
  = ZipConduit {getZipConduit :: ConduitT i o m r}

type role ZipSink nominal nominal representational
type ZipSink :: * -> (* -> *) -> * -> *
newtype ZipSink i m r = ZipSink {getZipSink :: Sink i m r}

type role ZipSource nominal nominal
type ZipSource :: (* -> *) -> * -> *
newtype ZipSource m o = ZipSource {getZipSource :: Source m o}




allC :: Monad m => (a -> Bool) -> ConduitT a o m Bool
allCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> Bool)
  -> ConduitT mono o m Bool
allNewBuffersStrategy :: Int -> BufferAllocStrategy
allocateAcquire ::
  MonadResource m =>
  Acquire a
  -> m (resourcet-1.2.4.2:Control.Monad.Trans.Resource.Internal.ReleaseKey,
        a)
andC :: Monad m => ConduitT Bool o m Bool
andCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   ~ Bool) =>
  ConduitT mono o m Bool
anyC :: Monad m => (a -> Bool) -> ConduitT a o m Bool
anyCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> Bool)
  -> ConduitT mono o m Bool
asumC ::
  (Monad m, GHC.Base.Alternative f) => ConduitT (f a) o m (f a)
await :: Monad m => Consumer i m (Maybe i)
awaitForever ::
  Monad m => (i -> ConduitT i o m r) -> ConduitT i o m ()
awaitNonNull ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable a) =>
  ConduitT
    a o m (Maybe (mono-traversable-1.0.15.1:Data.NonNull.NonNull a))
bracketP ::
  MonadResource m =>
  IO a -> (a -> IO ()) -> (a -> ConduitT i o m r) -> ConduitT i o m r
builderToByteString ::
  PrimMonad m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
    m
    ()
builderToByteStringFlush ::
  PrimMonad m =>
  ConduitT
    (Flush
       bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder)
    (Flush bytestring-0.10.12.1:Data.ByteString.Internal.ByteString)
    m
    ()
builderToByteStringWith ::
  PrimMonad m =>
  BufferAllocStrategy
  -> ConduitT
       bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
       m
       ()
builderToByteStringWithFlush ::
  PrimMonad m =>
  BufferAllocStrategy
  -> ConduitT
       (Flush
          bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder)
       (Flush bytestring-0.10.12.1:Data.ByteString.Internal.ByteString)
       m
       ()
catchC ::
  (MonadUnliftIO m, GHC.Exception.Type.Exception e) =>
  ConduitT i o m r -> (e -> ConduitT i o m r) -> ConduitT i o m r
catchCatchC ::
  Monad m =>
  ConduitT
    i o (exceptions-0.10.4:Control.Monad.Catch.Pure.CatchT m) r
  -> (GHC.Exception.Type.SomeException
      -> ConduitT
           i o (exceptions-0.10.4:Control.Monad.Catch.Pure.CatchT m) r)
  -> ConduitT
       i o (exceptions-0.10.4:Control.Monad.Catch.Pure.CatchT m) r
catchExceptC ::
  Monad m =>
  ConduitT i o (Control.Monad.Trans.Except.ExceptT e m) r
  -> (e -> ConduitT i o (Control.Monad.Trans.Except.ExceptT e m) r)
  -> ConduitT i o (Control.Monad.Trans.Except.ExceptT e m) r
chunksOfCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  mono-traversable-1.0.15.1:Data.Sequences.Index seq
  -> ConduitT seq seq m ()
chunksOfExactlyCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  mono-traversable-1.0.15.1:Data.Sequences.Index seq
  -> ConduitT seq seq m ()
concatC ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  ConduitT
    mono
    (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
    m
    ()
concatMapAccumC ::
  Monad m =>
  (a -> accum -> (accum, [b])) -> accum -> ConduitT a b m ()
concatMapAccumMC ::
  Monad m =>
  (a -> accum -> m (accum, [b])) -> accum -> ConduitT a b m ()
concatMapC ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (a -> mono)
  -> ConduitT
       a
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
       m
       ()
concatMapCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Monoid w) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono -> w)
  -> ConduitT mono w m ()
concatMapMC ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (a -> m mono)
  -> ConduitT
       a
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
       m
       ()
conduitVector ::
  (vector-0.12.3.0:Data.Vector.Generic.Base.Vector v a,
   PrimMonad m) =>
  Int -> ConduitT a (v a) m ()
connect ::
  Monad m => ConduitT () a m () -> ConduitT a Void m r -> m r
decodeUtf8C ::
  MonadThrow m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
    Data.Text.Internal.Text
    m
    ()
decodeUtf8LenientC ::
  Monad m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
    Data.Text.Internal.Text
    m
    ()
dropC :: Monad m => Int -> ConduitT a o m ()
dropCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  mono-traversable-1.0.15.1:Data.Sequences.Index seq
  -> ConduitT seq o m ()
dropWhileC :: Monad m => (a -> Bool) -> ConduitT a o m ()
dropWhileCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   -> Bool)
  -> ConduitT seq o m ()
elemC :: (Monad m, Eq a) => a -> ConduitT a o m Bool
elemCE ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   Eq (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq)) =>
  mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
  -> ConduitT seq o m Bool
encodeUtf8C ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.Utf8 text binary) =>
  ConduitT text binary m ()
enumFromToC ::
  (Monad m, Enum a, Ord a) => a -> a -> ConduitT i a m ()
evalRWSC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Strict.RWST r w s m) res
  -> ConduitT i o m (res, w)
evalRWSLC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Lazy.RWST r w s m) res
  -> ConduitT i o m (res, w)
evalStateC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Strict.StateT s m) r
  -> ConduitT i o m r
evalStateLC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Lazy.StateT s m) r
  -> ConduitT i o m r
exceptC ::
  Monad m =>
  ConduitT i o m (Either e a)
  -> ConduitT i o (Control.Monad.Trans.Except.ExceptT e m) a
execRWSC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Strict.RWST r w s m) res
  -> ConduitT i o m (s, w)
execRWSLC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Lazy.RWST r w s m) res
  -> ConduitT i o m (s, w)
execStateC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Strict.StateT s m) r
  -> ConduitT i o m s
execStateLC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Lazy.StateT s m) r
  -> ConduitT i o m s
execWriterC ::
  (Monad m, Monoid w) =>
  ConduitT i o (Control.Monad.Trans.Writer.Strict.WriterT w m) r
  -> ConduitT i o m w
execWriterLC ::
  (Monad m, Monoid w) =>
  ConduitT i o (Control.Monad.Trans.Writer.Lazy.WriterT w m) r
  -> ConduitT i o m w
filterC :: Monad m => (a -> Bool) -> ConduitT a a m ()
filterCE ::
  (mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   Monad m) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   -> Bool)
  -> ConduitT seq seq m ()
filterMC :: Monad m => (a -> m Bool) -> ConduitT a a m ()
filterMCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   -> m Bool)
  -> ConduitT seq seq m ()
findC :: Monad m => (a -> Bool) -> ConduitT a o m (Maybe a)
foldC :: (Monad m, Monoid a) => ConduitT a o m a
foldCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Monoid
     (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)) =>
  ConduitT
    mono
    o
    m
    (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
foldMC :: Monad m => (a -> b -> m a) -> a -> ConduitT b o m a
foldMCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (a
   -> mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> m a)
  -> a -> ConduitT mono o m a
foldMapC :: (Monad m, Monoid b) => (a -> b) -> ConduitT a o m b
foldMapCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Monoid w) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono -> w)
  -> ConduitT mono o m w
foldMapMC :: (Monad m, Monoid w) => (a -> m w) -> ConduitT a o m w
foldMapMCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Monoid w) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> m w)
  -> ConduitT mono o m w
foldlC :: Monad m => (a -> b -> a) -> a -> ConduitT b o m a
foldlCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (a
   -> mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> a)
  -> a -> ConduitT mono o m a
fuse ::
  Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
fuseBoth ::
  Monad m =>
  ConduitT a b m r1 -> ConduitT b c m r2 -> ConduitT a c m (r1, r2)
fuseBothMaybe ::
  Monad m =>
  ConduitT a b m r1
  -> ConduitT b c m r2 -> ConduitT a c m (Maybe r1, r2)
fuseLeftovers ::
  Monad m =>
  ([b] -> [a])
  -> ConduitT a b m () -> ConduitT b c m r -> ConduitT a c m r
fuseReturnLeftovers ::
  Monad m =>
  ConduitT a b m () -> ConduitT b c m r -> ConduitT a c m (r, [b])
fuseUpstream ::
  Monad m => ConduitT a b m r -> Conduit b m c -> ConduitT a c m r
handleC ::
  (MonadUnliftIO m, GHC.Exception.Type.Exception e) =>
  (e -> ConduitT i o m r) -> ConduitT i o m r -> ConduitT i o m r
headC :: Monad m => ConduitT a o m (Maybe a)
headCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  ConduitT
    seq
    o
    m
    (Maybe
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq))
headDefC :: Monad m => a -> ConduitT a o m a
intersperseC :: Monad m => a -> ConduitT a a m ()
iterMC :: Monad m => (a -> m ()) -> ConduitT a a m ()
iterateC :: Monad m => (a -> a) -> a -> ConduitT i a m ()
lastC :: Monad m => ConduitT a o m (Maybe a)
lastCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  ConduitT
    seq
    o
    m
    (Maybe
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq))
lastDefC :: Monad m => a -> ConduitT a o m a
leftover :: i -> ConduitT i o m ()
lengthC :: (Monad m, Num len) => ConduitT a o m len
lengthCE ::
  (Monad m, Num len,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  ConduitT mono o m len
lengthIfC ::
  (Monad m, Num len) => (a -> Bool) -> ConduitT a o m len
lengthIfCE ::
  (Monad m, Num len,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> Bool)
  -> ConduitT mono o m len
lineAsciiC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ GHC.Word.Word8) =>
  ConduitT seq o m r -> ConduitT seq o m r
lineC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ Char) =>
  ConduitT seq o m r -> ConduitT seq o m r
linesUnboundedAsciiC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ GHC.Word.Word8) =>
  ConduitT seq seq m ()
linesUnboundedC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ Char) =>
  ConduitT seq seq m ()
mapAccumS ::
  Monad m =>
  (a -> s -> ConduitT b Void m s)
  -> s -> ConduitT () b m () -> ConduitT a Void m s
mapAccumWhileC ::
  Monad m => (a -> s -> Either s (s, b)) -> s -> ConduitT a b m s
mapAccumWhileMC ::
  Monad m => (a -> s -> m (Either s (s, b))) -> s -> ConduitT a b m s
mapC :: Monad m => (a -> b) -> ConduitT a b m ()
mapCE ::
  (Monad m, Functor f) => (a -> b) -> ConduitT (f a) (f b) m ()
mapInput ::
  Monad m =>
  (i1 -> i2)
  -> (i2 -> Maybe i1) -> ConduitT i2 o m r -> ConduitT i1 o m r
mapInputM ::
  Monad m =>
  (i1 -> m i2)
  -> (i2 -> m (Maybe i1)) -> ConduitT i2 o m r -> ConduitT i1 o m r
mapMC :: Monad m => (a -> m b) -> ConduitT a b m ()
mapMCE ::
  (Monad m, Traversable f) => (a -> m b) -> ConduitT (f a) (f b) m ()
mapM_C :: Monad m => (a -> m ()) -> ConduitT a o m ()
mapM_CE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> m ())
  -> ConduitT mono o m ()
mapOutput ::
  Monad m => (o1 -> o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutputMaybe ::
  Monad m =>
  (o1 -> Maybe o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapWhileC :: Monad m => (a -> Maybe b) -> ConduitT a b m ()
maximumC :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
maximumCE ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   Ord
     (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq)) =>
  ConduitT
    seq
    o
    m
    (Maybe
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq))
maybeC ::
  Monad m =>
  ConduitT i o m (Maybe a)
  -> ConduitT i o (Control.Monad.Trans.Maybe.MaybeT m) a
mergeSource :: Monad m => Source m i -> Conduit a m (i, a)
minimumC :: (Monad m, Ord a) => ConduitT a o m (Maybe a)
minimumCE ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   Ord
     (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq)) =>
  ConduitT
    seq
    o
    m
    (Maybe
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq))
mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquireType :: IO a -> (a -> ReleaseType -> IO ()) -> Acquire a
notElemC :: (Monad m, Eq a) => a -> ConduitT a o m Bool
notElemCE ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   Eq (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq)) =>
  mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
  -> ConduitT seq o m Bool
nullC :: Monad m => ConduitT a o m Bool
nullCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  ConduitT mono o m Bool
omapCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFunctor mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
  -> ConduitT mono mono m ()
omapMCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoTraversable
     mono) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   -> m (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono))
  -> ConduitT mono mono m ()
orC :: Monad m => ConduitT Bool o m Bool
orCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono
   ~ Bool) =>
  ConduitT mono o m Bool
passthroughSink ::
  Monad m => Sink i m r -> (r -> m ()) -> Conduit i m i
peekC :: Monad m => ConduitT a o m (Maybe a)
peekCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable
     mono) =>
  ConduitT
    mono
    o
    m
    (Maybe
       (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono))
peekForever :: Monad m => ConduitT i o m () -> ConduitT i o m ()
peekForeverE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable i) =>
  ConduitT i o m () -> ConduitT i o m ()
printC :: (Show a, MonadIO m) => ConduitT a o m ()
productC :: (Monad m, Num a) => ConduitT a o m a
productCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Num
     (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)) =>
  ConduitT
    mono
    o
    m
    (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
readerC ::
  Monad m =>
  (r -> ConduitT i o m a)
  -> ConduitT i o (Control.Monad.Trans.Reader.ReaderT r m) a
repeatC :: Monad m => a -> ConduitT i a m ()
repeatMC :: Monad m => m a -> ConduitT i a m ()
repeatWhileMC :: Monad m => m a -> (a -> Bool) -> ConduitT i a m ()
replicateC :: Monad m => Int -> a -> ConduitT i a m ()
replicateMC :: Monad m => Int -> m a -> ConduitT i a m ()
reuseBufferStrategy ::
  IO Data.Conduit.Combinators.Buffer -> BufferAllocStrategy
runCatchC ::
  Monad m =>
  ConduitT
    i o (exceptions-0.10.4:Control.Monad.Catch.Pure.CatchT m) r
  -> ConduitT i o m (Either GHC.Exception.Type.SomeException r)
runConduit :: Monad m => ConduitT () Void m r -> m r
runConduitPure :: ConduitT () Void Identity r -> r
runConduitRes ::
  MonadUnliftIO m => ConduitT () Void (ResourceT m) r -> m r
runExceptC ::
  Monad m =>
  ConduitT i o (Control.Monad.Trans.Except.ExceptT e m) r
  -> ConduitT i o m (Either e r)
runMaybeC ::
  Monad m =>
  ConduitT i o (Control.Monad.Trans.Maybe.MaybeT m) r
  -> ConduitT i o m (Maybe r)
runRWSC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Strict.RWST r w s m) res
  -> ConduitT i o m (res, s, w)
runRWSLC ::
  (Monad m, Monoid w) =>
  r
  -> s
  -> ConduitT i o (Control.Monad.Trans.RWS.Lazy.RWST r w s m) res
  -> ConduitT i o m (res, s, w)
runReaderC ::
  Monad m =>
  r
  -> ConduitT i o (Control.Monad.Trans.Reader.ReaderT r m) res
  -> ConduitT i o m res
runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runStateC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Strict.StateT s m) r
  -> ConduitT i o m (r, s)
runStateLC ::
  Monad m =>
  s
  -> ConduitT i o (Control.Monad.Trans.State.Lazy.StateT s m) r
  -> ConduitT i o m (r, s)
runWriterC ::
  (Monad m, Monoid w) =>
  ConduitT i o (Control.Monad.Trans.Writer.Strict.WriterT w m) r
  -> ConduitT i o m (r, w)
runWriterLC ::
  (Monad m, Monoid w) =>
  ConduitT i o (Control.Monad.Trans.Writer.Lazy.WriterT w m) r
  -> ConduitT i o m (r, w)
rwsC ::
  (Monad m, Monoid w) =>
  (r -> s -> ConduitT i o m (a, s, w))
  -> ConduitT i o (Control.Monad.Trans.RWS.Strict.RWST r w s m) a
rwsLC ::
  (Monad m, Monoid w) =>
  (r -> s -> ConduitT i o m (a, s, w))
  -> ConduitT i o (Control.Monad.Trans.RWS.Lazy.RWST r w s m) a
scanlC :: Monad m => (a -> b -> a) -> a -> ConduitT b a m ()
scanlMC :: Monad m => (a -> b -> m a) -> a -> ConduitT b a m ()
sealConduitT :: ConduitT i o m r -> SealedConduitT i o m r
sequenceConduits ::
  (Traversable f, Monad m) =>
  f (ConduitT i o m r) -> ConduitT i o m (f r)
sequenceSinks ::
  (Traversable f, Monad m) => f (Sink i m r) -> Sink i m (f r)
sequenceSources ::
  (Traversable f, Monad m) => f (Source m o) -> Source m (f o)
sinkFile ::
  MonadResource m =>
  FilePath
  -> ConduitT
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sinkFileBS ::
  MonadResource m =>
  FilePath
  -> ConduitT
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sinkFileCautious ::
  MonadResource m =>
  FilePath
  -> ConduitM
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sinkHandle ::
  MonadIO m =>
  GHC.IO.Handle.Types.Handle
  -> ConduitT
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sinkHandleBuilder ::
  MonadIO m =>
  GHC.IO.Handle.Types.Handle
  -> ConduitM
       bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
       o
       m
       ()
sinkHandleFlush ::
  MonadIO m =>
  GHC.IO.Handle.Types.Handle
  -> ConduitM
       (Flush bytestring-0.10.12.1:Data.ByteString.Internal.ByteString)
       o
       m
       ()
sinkIOHandle ::
  MonadResource m =>
  IO GHC.IO.Handle.Types.Handle
  -> ConduitT
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sinkLazy ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.LazySequence
     lazy strict) =>
  ConduitT strict o m lazy
sinkLazyBuilder ::
  Monad m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
    o
    m
    bytestring-0.10.12.1:Data.ByteString.Lazy.Internal.ByteString
sinkList :: Monad m => ConduitT a o m [a]
sinkNull :: Monad m => ConduitT a o m ()
sinkSystemTempFile ::
  MonadResource m =>
  String
  -> ConduitM
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
       o
       m
       FilePath
sinkTempFile ::
  MonadResource m =>
  FilePath
  -> String
  -> ConduitM
       bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
       o
       m
       FilePath
sinkVector ::
  (vector-0.12.3.0:Data.Vector.Generic.Base.Vector v a,
   PrimMonad m) =>
  ConduitT a o m (v a)
sinkVectorN ::
  (vector-0.12.3.0:Data.Vector.Generic.Base.Vector v a,
   PrimMonad m) =>
  Int -> ConduitT a o m (v a)
slidingWindowC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq ~ a) =>
  Int -> ConduitT a seq m ()
sourceDirectory ::
  MonadResource m => FilePath -> ConduitT i FilePath m ()
sourceDirectoryDeep ::
  MonadResource m => Bool -> FilePath -> ConduitT i FilePath m ()
sourceFile ::
  MonadResource m =>
  FilePath
  -> ConduitT
       i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
sourceFileBS ::
  MonadResource m =>
  FilePath
  -> ConduitT
       i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
sourceHandle ::
  MonadIO m =>
  GHC.IO.Handle.Types.Handle
  -> ConduitT
       i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
sourceHandleUnsafe ::
  MonadIO m =>
  GHC.IO.Handle.Types.Handle
  -> ConduitT
       i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
sourceIOHandle ::
  MonadResource m =>
  IO GHC.IO.Handle.Types.Handle
  -> ConduitT
       i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
sourceLazy ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.LazySequence
     lazy strict) =>
  lazy -> ConduitT i strict m ()
sourceToList :: Monad m => Source m a -> m [a]
stateC ::
  Monad m =>
  (s -> ConduitT i o m (a, s))
  -> ConduitT i o (Control.Monad.Trans.State.Strict.StateT s m) a
stateLC ::
  Monad m =>
  (s -> ConduitT i o m (a, s))
  -> ConduitT i o (Control.Monad.Trans.State.Lazy.StateT s m) a
stderrC ::
  MonadIO m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
stdinC ::
  MonadIO m =>
  ConduitT
    i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString m ()
stdoutC ::
  MonadIO m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o m ()
sumC :: (Monad m, Num a) => ConduitT a o m a
sumCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono,
   Num
     (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)) =>
  ConduitT
    mono
    o
    m
    (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono)
takeC :: Monad m => Int -> ConduitT a a m ()
takeCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  mono-traversable-1.0.15.1:Data.Sequences.Index seq
  -> ConduitT seq seq m ()
takeExactlyC ::
  Monad m => Int -> ConduitT a b m r -> ConduitT a b m r
takeExactlyCE ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence a) =>
  mono-traversable-1.0.15.1:Data.Sequences.Index a
  -> ConduitT a b m r -> ConduitT a b m r
takeWhileC :: Monad m => (a -> Bool) -> ConduitT a a m ()
takeWhileCE ::
  (Monad m,
   mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq) =>
  (mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   -> Bool)
  -> ConduitT seq seq m ()
toConsumer :: Monad m => Sink a m b -> Consumer a m b
toProducer :: Monad m => Source m a -> ConduitT i a m ()
transPipe ::
  Monad m =>
  (forall a. m a -> n a) -> ConduitT i o m r -> ConduitT i o n r
tryC ::
  (MonadUnliftIO m, GHC.Exception.Type.Exception e) =>
  ConduitT i o m r -> ConduitT i o m (Either e r)
unfoldC :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
unlinesAsciiC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ GHC.Word.Word8) =>
  ConduitT seq seq m ()
unlinesC ::
  (Monad m, mono-traversable-1.0.15.1:Data.Sequences.IsSequence seq,
   mono-traversable-1.0.15.1:Data.MonoTraversable.Element seq
   ~ Char) =>
  ConduitT seq seq m ()
unsafeBuilderToByteString ::
  PrimMonad m =>
  ConduitT
    bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
    bytestring-0.10.12.1:Data.ByteString.Internal.ByteString
    m
    ()
unsealConduitT ::
  Monad m => SealedConduitT i o m r -> ConduitT i o m r
vectorBuilderC ::
  (PrimMonad m, vector-0.12.3.0:Data.Vector.Generic.Base.Vector v e,
   PrimMonad n, PrimState m ~ PrimState n) =>
  Int -> ((e -> n ()) -> ConduitT i Void m r) -> ConduitT i (v e) m r
withAcquire :: MonadUnliftIO m => Acquire a -> (a -> m b) -> m b
withSinkFile ::
  (MonadUnliftIO m, MonadIO n) =>
  FilePath
  -> (ConduitM
        bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o n ()
      -> m a)
  -> m a
withSinkFileBuilder ::
  (MonadUnliftIO m, MonadIO n) =>
  FilePath
  -> (ConduitM
        bytestring-0.10.12.1:Data.ByteString.Builder.Internal.Builder
        o
        n
        ()
      -> m a)
  -> m a

withSinkFileCautious :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitM bytestring-0.10.12.1:Data.ByteString.Internal.ByteString o n () -> m a)
  -> m a

withSourceFile :: (MonadUnliftIO m, MonadIO n)
               => FilePath
               -> (ConduitM i bytestring-0.10.12.1:Data.ByteString.Internal.ByteString n () -> m a)
               -> m a

writerC :: (Monad m, Monoid w)
        => ConduitT i o m (b, w)
        -> ConduitT i o (Control.Monad.Trans.Writer.Strict.WriterT w m) b

writerLC :: (Monad m, Monoid w)
         => ConduitT i o m (b, w)
         -> ConduitT i o (Control.Monad.Trans.Writer.Lazy.WriterT w m) b

yield     :: Monad m =>   o -> ConduitT i o m ()
yieldM    :: Monad m => m o -> ConduitT i o m ()
yieldMany :: (Monad m, mono-traversable-1.0.15.1:Data.MonoTraversable.MonoFoldable mono)
          => mono
          -> ConduitT i (mono-traversable-1.0.15.1:Data.MonoTraversable.Element mono) m ()
```
