# Control.Monad.Trans.Cont

```hs
import Control.Monad.Trans.Cont

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- (1)
type Cont r a = ContT r Identity a
-- (2)
type Cont r a =  Cont { runCont :: (a -> r) -> r }


type Cont :: Type -> Type -> Type
type Cont r = ContT @{Type} r Identity :: Type -> Type

type role ContT nominal nominal representational representational
type ContT :: forall {k}. k -> (k -> Type) -> Type -> Type
newtype ContT @{k} r m a = ContT {runContT :: (a -> m r) -> m r}

Control.Monad.Trans.Cont.callCC ::
  forall {k} a (r :: k) (m :: k -> Type) b.
  ((a -> ContT @{k} r m b) -> ContT @{k} r m a) -> ContT @{k} r m a

cont :: forall a r. ((a -> r) -> r) -> Cont r a

evalCont :: forall r. Cont r r -> r

evalContT ::
  forall (m :: Type -> Type) r. Monad m => ContT @{Type} r m r -> m r

liftLocal ::
  forall (m :: Type -> Type) r' r a.
  Monad m =>
  m r'
  -> ((r' -> r') -> m r -> m r)
  -> (r' -> r')
  -> ContT @{Type} r m a
  -> ContT @{Type} r m a

mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a

mapContT ::
  forall {k} (m :: k -> Type) (r :: k) a.
  (m r -> m r) -> ContT @{k} r m a -> ContT @{k} r m a

reset :: forall r r'. Cont r r -> Cont r' r

resetT ::
  forall (m :: Type -> Type) r r'.
  Monad m =>
  ContT @{Type} r m r -> ContT @{Type} r' m r

runCont :: forall r a. Cont r a -> (a -> r) -> r

shift :: forall a r. ((a -> r) -> Cont r r) -> Cont r a

shiftT ::
  forall (m :: Type -> Type) a r.
  Monad m =>
  ((a -> m r) -> ContT @{Type} r m r) -> ContT @{Type} r m a

withCont ::
  forall b r a. ((b -> r) -> a -> r) -> Cont r a -> Cont r b

withContT ::
  forall {k} b (m :: k -> Type) (r :: k) a.
  ((b -> m r) -> a -> m r) -> ContT @{k} r m a -> ContT @{k} r m b
```
