# Control.Monad.Trans.Accum

https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Accum.html


```hs
type Accum :: * -> * -> *
type Accum w = AccumT w Data.Functor.Identity.Identity :: * -> *

type role AccumT nominal representational nominal
type      AccumT :: * -> (* -> *) -> * -> *
newtype   AccumT w m a = AccumT (w -> m (a, w))


evalAccum  :: Monoid w =>                        Accum  w   a -> w -> a
execAccum  ::                                    Accum  w   a -> w -> w
add        :: Monad  m =>                   w -> AccumT w m ()
execAccumT :: Monad  m =>                        AccumT w m a -> w -> m w
evalAccumT :: (Monad m, Monoid w) =>             AccumT w m a -> w -> m a
look  :: (Monoid w, Monad m) =>                  AccumT w m w
looks :: (Monoid w, Monad m) =>      (w -> a) -> AccumT w m a
accum :: Monad m             => (w -> (a, w)) -> AccumT w m a

mapAccum  :: (  (a, w) ->   (b, w)) -> Accum  w   a -> Accum  w   b
mapAccumT :: (m (a, w) -> n (b, w)) -> AccumT w m a -> AccumT w n b

runAccum  :: Accum  w   a -> w ->   (a, w)
runAccumT :: AccumT w m a -> w -> m (a, w)


readerToAccumT :: (Functor m, Monoid w)
                    => Control.Monad.Trans.Reader.ReaderT w m a -> AccumT w m a

writerToAccumT :: Control.Monad.Trans.Writer.Lazy.WriterT w m a -> AccumT w m a

accumToStateT :: (Functor m, Monoid s) 
              => AccumT s m a 
              -> Control.Monad.Trans.State.Lazy.StateT s m a


liftCallCC :: Control.Monad.Signatures.CallCC m (a, w) (b, w)
           -> Control.Monad.Signatures.CallCC (AccumT w m) a b

liftCallCC' :: Control.Monad.Signatures.CallCC m (a, w) (b, w)
            -> Control.Monad.Signatures.CallCC (AccumT w m) a b

liftCatch :: Control.Monad.Signatures.Catch e m (a, w)
          -> Control.Monad.Signatures.Catch e (AccumT w m) a

liftListen :: Monad m
           => Control.Monad.Signatures.Listen w m (a, s)
           -> Control.Monad.Signatures.Listen w (AccumT s m) a

liftPass :: Monad m 
         => Control.Monad.Signatures.Pass w m (a, s)
         -> Control.Monad.Signatures.Pass w (AccumT s m) a
```
