# Control.Monad.Trans.List

```hs
type role ListT representational nominal
type      ListT :: (* -> *) -> * -> *
newtype   ListT m a = ListT { runListT :: m [a] }

mapListT :: (m [a] -> n [b])
         -> ListT m a
         -> ListT n b

liftCallCC :: Control.Monad.Signatures.CallCC m [a] [b]
           -> Control.Monad.Signatures.CallCC (ListT m) a b

liftCatch :: Control.Monad.Signatures.Catch e m [a]
          -> Control.Monad.Signatures.Catch e (ListT m) a
```
