# Control.Monad.Trans.Identity

```hs
type role IdentityT representational nominal
type      IdentityT :: forall {k}. (k -> *) -> k -> *
newtype   IdentityT f a = IdentityT { runIdentityT :: f a }
runIdentityT :: forall {k} (f :: k -> *) (a :: k). IdentityT f a -> f a

IdentityT :: forall k (f :: k -> *) (a :: k). f a -> IdentityT      f a
--        :: forall   (f :: * -> *)  a      . f a -> IdentityT @{*} f a


liftCallCC :: Control.Monad.Signatures.CallCC m a b
           -> Control.Monad.Signatures.CallCC (IdentityT m) a b

liftCatch :: forall {k} e (m :: k -> *) (a :: k)
          .  Control.Monad.Signatures.Catch e m a
          -> Control.Monad.Signatures.Catch e (IdentityT m) a

mapIdentityT
  :: forall {k1} {k2}
     (m₁ :: k1 -> *) (a :: k1)
     (m₂ :: k2 -> *) (b :: k2)
  .  (m₁ a -> m₂ b)
  -> IdentityT m₁ a
  -> IdentityT m₂ b

```
