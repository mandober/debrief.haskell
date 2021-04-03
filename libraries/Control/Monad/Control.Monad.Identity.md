# Control.Monad.Identity

> :browse! Control.Monad.Identity

```hs
-- Control.Monad.(<$!>)
(<$!>) :: forall (m :: * -> *) a b. Monad m => (a -> b) -> m a -> m b

(Control.Monad.<=<) :: forall (m :: * -> *) b c a. Monad m =>
  (b -> m c) -> (a -> m b) -> a -> m c

(Control.Monad.>=>) :: forall (m :: * -> *) a b c. Monad m =>
  (a -> m b) -> (b -> m c) -> a -> m c


type    Identity :: * -> *
newtype Identity a = Identity { runIdentity :: a }

Identity :: forall a. a -> Identity a


type Control.Monad.Fix.MonadFix :: (* -> *) -> Constraint
class Monad m => Control.Monad.Fix.MonadFix m
  ...

type GHC.Base.MonadPlus :: (* -> *) -> Constraint
class (GHC.Base.Alternative m, Monad m) => GHC.Base.MonadPlus m
  ...


GHC.Base.ap ::
  forall (m :: * -> *) a b. Monad m => m (a -> b) -> m a -> m b
Control.Monad.filterM ::
  forall (m :: * -> *) a.
  Applicative m =>
  (a -> m Bool) -> [a] -> m [a]
Data.Function.fix :: forall a. (a -> a) -> a
Control.Monad.foldM ::
  forall (t :: * -> *) (m :: * -> *) b a.
  (Foldable t, Monad m) =>
  (b -> a -> m b) -> b -> t a -> m b
Control.Monad.foldM_ ::
  forall (t :: * -> *) (m :: * -> *) b a.
  (Foldable t, Monad m) =>
  (b -> a -> m b) -> b -> t a -> m ()
Data.Traversable.forM ::
  forall (t :: * -> *) (m :: * -> *) a b.
  (Traversable t, Monad m) =>
  t a -> (a -> m b) -> m (t b)
Data.Foldable.forM_ ::
  forall (t :: * -> *) (m :: * -> *) a b.
  (Foldable t, Monad m) =>
  t a -> (a -> m b) -> m ()
Control.Monad.forever ::
  forall (f :: * -> *) a b. Applicative f => f a -> f b
Control.Monad.guard ::
  forall (f :: * -> *). GHC.Base.Alternative f => Bool -> f ()
GHC.Base.join :: forall (m :: * -> *) a. Monad m => m (m a) -> m a
GHC.Base.liftM ::
  forall (m :: * -> *) a1 r. Monad m => (a1 -> r) -> m a1 -> m r
GHC.Base.liftM2 ::
  forall (m :: * -> *) a1 a2 r.
  Monad m =>
  (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
GHC.Base.liftM3 ::
  forall (m :: * -> *) a1 a2 a3 r.
  Monad m =>
  (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
GHC.Base.liftM4 ::
  forall (m :: * -> *) a1 a2 a3 a4 r.
  Monad m =>
  (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
GHC.Base.liftM5 ::
  forall (m :: * -> *) a1 a2 a3 a4 a5 r.
  Monad m =>
  (a1 -> a2 -> a3 -> a4 -> a5 -> r)
  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
Control.Monad.mapAndUnzipM ::
  forall (m :: * -> *) a b c.
  Applicative m =>
  (a -> m (b, c)) -> [a] -> m ([b], [c])
Control.Monad.mfilter ::
  forall (m :: * -> *) a.
  GHC.Base.MonadPlus m =>
  (a -> Bool) -> m a -> m a
Control.Monad.Fix.mfix ::
  forall (m :: * -> *) a.
  Control.Monad.Fix.MonadFix m =>
  (a -> m a) -> m a
GHC.Base.mplus ::
  forall (m :: * -> *) a. GHC.Base.MonadPlus m => m a -> m a -> m a
Data.Foldable.msum ::
  forall (t :: * -> *) (m :: * -> *) a.
  (Foldable t, GHC.Base.MonadPlus m) =>
  t (m a) -> m a
GHC.Base.mzero ::
  forall (m :: * -> *) a. GHC.Base.MonadPlus m => m a
Control.Monad.replicateM ::
  forall (m :: * -> *) a. Applicative m => Int -> m a -> m [a]
Control.Monad.replicateM_ ::
  forall (m :: * -> *) a. Applicative m => Int -> m a -> m ()
runIdentity ::
  forall a. Identity a -> a
Control.Monad.unless ::
  forall (f :: * -> *). Applicative f => Bool -> f () -> f ()
Data.Functor.void ::
  forall (f :: * -> *) a. Functor f => f a -> f ()
GHC.Base.when ::
  forall (f :: * -> *). Applicative f => Bool -> f () -> f ()
Control.Monad.zipWithM ::
  forall (m :: * -> *) a b c.
  Applicative m =>
  (a -> b -> m c) -> [a] -> [b] -> m [c]
Control.Monad.zipWithM_ ::
  forall (m :: * -> *) a b c.
  Applicative m =>
  (a -> b -> m c) -> [a] -> [b] -> m ()


-- imported via Control.Monad.Trans.Identity
IdentityT :: forall {k} (f :: k -> *) (a :: k). f a -> IdentityT @{k} f a
type role IdentityT nominal representational nominal
type IdentityT :: forall {k}. (k -> *) -> k -> *

type role IdentityT nominal representational nominal
type IdentityT :: forall {k}. (k -> *) -> k -> *
newtype IdentityT @{k} f a = IdentityT {runIdentityT :: f a}
-- IdentityT :: forall {f :: * -> *} {a}. f a -> IdentityT @{*} f a


liftCallCC ::
  forall (m :: * -> *) a b.
  Control.Monad.Signatures.CallCC m a b
  -> Control.Monad.Signatures.CallCC (IdentityT @{*} m) a b
liftCatch ::
  forall {k} e (m :: k -> *) (a :: k).
  Control.Monad.Signatures.Catch @{k} e m a
  -> Control.Monad.Signatures.Catch @{k} e (IdentityT @{k} m) a
mapIdentityT ::
  forall {k1} {k2} (m :: k1 -> *) (a :: k1) (n :: k2 -> *) (b :: k2).
  (m a -> n b) -> IdentityT @{k1} m a -> IdentityT @{k2} n b
runIdentityT ::
  forall {k} (f :: k -> *) (a :: k). IdentityT @{k} f a -> f a
-- imported via Prelude
(<$) :: forall (f :: * -> *) a b. Functor f => a -> f b -> f a
(=<<) ::
  forall (m :: * -> *) a b. Monad m => (a -> m b) -> m a -> m b
(>>) :: forall (m :: * -> *) a b. Monad m => m a -> m b -> m b
(>>=) ::
  forall (m :: * -> *) a b. Monad m => m a -> (a -> m b) -> m b
type Functor :: (* -> *) -> Constraint
class Functor f
  ...
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m
  ...
type MonadFail :: (* -> *) -> Constraint
class Monad m => MonadFail m
  ...
fail :: forall (m :: * -> *) a. MonadFail m => String -> m a
fmap ::
  forall (f :: * -> *) a b. Functor f => (a -> b) -> f a -> f b
mapM ::
  forall (t :: * -> *) (m :: * -> *) a b.
  (Traversable t, Monad m) =>
  (a -> m b) -> t a -> m (t b)
mapM_ ::
  forall (t :: * -> *) (m :: * -> *) a b.
  (Foldable t, Monad m) =>
  (a -> m b) -> t a -> m ()
return :: forall (m :: * -> *) a. Monad m => a -> m a
sequence ::
  forall (t :: * -> *) (m :: * -> *) a.
  (Traversable t, Monad m) =>
  t (m a) -> m (t a)
sequence_ ::
  forall (t :: * -> *) (m :: * -> *) a.
  (Foldable t, Monad m) =>
  t (m a) -> m ()
```
