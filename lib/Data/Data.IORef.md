# Data.IORef

````hs
newIORef            ::                        a -> IO (IORef a)
readIORef           :: IORef a                  -> IO a
writeIORef          :: IORef a -> a             -> IO ()
atomicWriteIORef    :: IORef a -> a             -> IO ()
modifyIORef         :: IORef a -> (a -> a)      -> IO ()
modifyIORef'        :: IORef a -> (a -> a)      -> IO ()

atomicModifyIORef   :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef'  :: IORef a -> (a -> (a, b)) -> IO b

mkWeakIORef         :: IORef a         -> IO () -> IO (GHC.Weak.Weak (IORef a))

type    IORef :: * -> *
newtype IORef a = IORef (STRef RealWorld a)
-- fully-qualified type
newtype IORef a =
  GHC.IORef.IORef (GHC.STRef.STRef ghc-prim-0.7.0:GHC.Prim.RealWorld a)
```
