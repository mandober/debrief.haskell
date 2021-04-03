# Control.Monad.Trans.Except

```hs
type Except :: * -> * -> *
type Except e = ExceptT e Data.Functor.Identity.Identity :: * -> *

type role ExceptT nominal representational nominal
type      ExceptT :: * -> (* -> *) -> * -> *
newtype   ExceptT e m a = ExceptT (m (Either e a))

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
except :: Monad m => Either e a -> ExceptT e m a

liftCallCC :: Control.Monad.Signatures.CallCC m (Either e a) (Either e b)
           -> Control.Monad.Signatures.CallCC (ExceptT e m) a b

liftListen :: Monad m
           => Control.Monad.Signatures.Listen w m (Either e a)
           -> Control.Monad.Signatures.Listen w (ExceptT e m) a

liftPass :: Monad m 
         => Control.Monad.Signatures.Pass w m (Either e a)
         -> Control.Monad.Signatures.Pass w (ExceptT e m) a

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b

mapExceptT :: (m (Either e a)
           -> n (Either e' b))
           -> ExceptT e m a
           -> ExceptT e' n b

runExcept  :: Except e a -> Either e a
runExceptT :: ExceptT e m a -> m (Either e a)

throwE :: Monad m => e -> ExceptT e m a

withExcept  :: (e -> e') -> Except e a -> Except e' a
withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
```
