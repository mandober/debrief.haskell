# Foreign.Storable

```hs
class Foreign.Storable.Storable a
  ...
Foreign.Storable.alignment ::
  Foreign.Storable.Storable a => a -> Int
Foreign.Storable.peek ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr a -> IO a
Foreign.Storable.peekByteOff ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr b -> Int -> IO a
Foreign.Storable.peekElemOff ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr a -> Int -> IO a
Foreign.Storable.poke ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr a -> a -> IO ()
Foreign.Storable.pokeByteOff ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr b -> Int -> a -> IO ()
Foreign.Storable.pokeElemOff ::
  Foreign.Storable.Storable a => GHC.Ptr.Ptr a -> Int -> a -> IO ()
Foreign.Storable.sizeOf :: Foreign.Storable.Storable a => a -> Int
```

File: Storable.hs
/home/ivan/code/haskell/gambit-hs/ghc/ghc9/libraries/base/Foreign/Storable.hs


```hs
class Storable a where
    sizeOf    :: a -> Int
    alignment :: a -> Int

    peek :: GHC.Ptr.Ptr a                    -> IO a
    poke :: GHC.Ptr.Ptr a -> a               -> IO ()

    peekElemOff :: GHC.Ptr.Ptr a -> Int      -> IO a
    pokeElemOff :: GHC.Ptr.Ptr a -> Int -> a -> IO ()

    peekByteOff :: GHC.Ptr.Ptr b -> Int      -> IO a
    pokeByteOff :: GHC.Ptr.Ptr b -> Int -> a -> IO ()

    {-# MINIMAL sizeOf, alignment,
    (peek | peekElemOff | peekByteOff),
    (poke | pokeElemOff | pokeByteOff) #-}


-- Defined in Foreign.Storable
instance Storable ()
instance Storable Float
instance Storable Double
instance Storable Char
instance Storable Bool

instance Storable Word
instance Storable GHC.Word.Word8
instance Storable GHC.Word.Word64
instance Storable GHC.Word.Word32
instance Storable GHC.Word.Word16

instance Storable Int
instance Storable GHC.Int.Int8
instance Storable GHC.Int.Int64
instance Storable GHC.Int.Int32
instance Storable GHC.Int.Int16

instance Storable (GHC.Ptr.Ptr a)
instance Storable (GHC.Ptr.FunPtr a)
instance Storable (GHC.Stable.StablePtr a)
instance Storable GHC.Fingerprint.Type.Fingerprint
instance (Storable a, Integral a) => Storable (GHC.Real.Ratio a)

-- Defined in Data.Functor.Identity
instance (Storable a) => Storable (Data.Functor.Identity.Identity a)

-- Defined in Data.Functor.Const
instance forall a k (b :: k). (Storable a) => 
    Storable (Data.Functor.Const.Const a b)
```
