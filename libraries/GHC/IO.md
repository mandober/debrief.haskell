# GHC.IO

```hs
import GHC.IO

data GHC.IO.MaskingState = ...ETC

GHC.IO.Unmasked              :: GHC.IO.MaskingState
GHC.IO.MaskedInterruptible   :: GHC.IO.MaskingState
GHC.IO.MaskedUninterruptible :: GHC.IO.MaskingState

GHC.IO.bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

GHC.IO.catch :: GHC.Exception.Type.Exception e
             => IO a -> (e -> IO a) -> IO a

GHC.IO.catchAny :: IO a
                -> (forall e. GHC.Exception.Type.Exception e => e -> IO a) 
                -> IO a

GHC.IO.catchException :: GHC.Exception.Type.Exception e
                      => IO a -> (e -> IO a) -> IO a

GHC.IO.evaluate         :: a -> IO a
GHC.IO.failIO           :: String -> IO a
GHC.IO.finally          :: IO a -> IO b -> IO a
GHC.IO.getMaskingState  :: IO GHC.IO.MaskingState
GHC.IO.interruptible    :: IO a -> IO a
GHC.IO.ioToST           :: IO a -> GHC.ST.ST GHC.Prim.RealWorld a

GHC.IO.liftIO :: IO a
  -> GHC.Prim.State# GHC.Prim.RealWorld
  -> GHC.ST.STret GHC.Prim.RealWorld a

GHC.IO.mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
GHC.IO.mask_ :: IO a -> IO a

GHC.IO.mplusIO     :: IO a -> IO a -> IO a
GHC.IO.onException :: IO a -> IO b -> IO a

GHC.IO.stToIO  :: GHC.ST.ST GHC.Prim.RealWorld a -> IO a
GHC.IO.throwIO :: GHC.Exception.Type.Exception e => e -> IO a

GHC.IO.uninterruptibleMask ::
  ((forall a. IO a -> IO a) -> IO b) -> IO b

GHC.IO.uninterruptibleMask_ :: IO a -> IO a

GHC.IO.unsafeUnmask :: IO a -> IO a
GHC.IO.unsafeIOToST :: IO a -> GHC.ST.ST s a
GHC.IO.unsafeSTToIO :: GHC.ST.ST s a -> IO a

GHC.Types.IO ::
        (GHC.Prim.State# GHC.Prim.RealWorld
   -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
   -> IO a

GHC.IO.Unsafe.noDuplicate :: IO ()

GHC.Base.unIO :: IO a
  ->    GHC.Prim.State# GHC.Prim.RealWorld
  -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #)


GHC.IO.Unsafe.unsafePerformIO           :: IO a -> a
GHC.IO.Unsafe.unsafeInterleaveIO        :: IO a -> IO a
GHC.IO.Unsafe.unsafeDupablePerformIO    :: IO a -> a
GHC.IO.Unsafe.unsafeDupableInterleaveIO :: IO a -> IO a

-- imported via Prelude
type FilePath = String
newtype IO a = ...
```
