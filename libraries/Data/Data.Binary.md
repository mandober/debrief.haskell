# Data.Binary

```hs
-- GHC.Types
-- GHC.Prim
-- GHC.Generics
-- Data.ByteString.Lazy.Internal

import Data.Binary


encodeFile :: Binary a => FilePath -> a -> IO ()
decodeFile :: Binary a => FilePath -> IO a

encode :: Binary a => a -> Data.ByteString.Lazy.Internal.ByteString
decode :: Binary a => Data.ByteString.Lazy.Internal.ByteString -> a

decodeFileOrFail :: Binary a => FilePath -> IO (Either (Data.Binary.Get.ByteOffset, String) a)

decodeOrFail :: Binary a => Data.ByteString.Lazy.Internal.ByteString
    -> Either
    (Data.ByteString.Lazy.Internal.ByteString, Data.Binary.Get.ByteOffset, String)
    (Data.ByteString.Lazy.Internal.ByteString, Data.Binary.Get.ByteOffset, a)

newtype Get a = Data.Binary.Get.Internal.C
    { Data.Binary.Get.Internal.runCont
    :: forall r.Data.ByteString.Internal.ByteString
    -> Data.Binary.Get.Internal.Success a r
    -> Data.Binary.Get.Internal.Decoder r
    }

type Put = Data.Binary.Put.PutM ()

data Word   = GHC.Types.W# GHC.Prim.Word#
data Word8  = GHC.Word.W8# GHC.Prim.Word#
data Word16 = GHC.Word.W16# GHC.Prim.Word#
data Word32 = GHC.Word.W32# GHC.Prim.Word#
data Word64 = GHC.Word.W64# GHC.Prim.Word#

byteSwap16 :: Word16 -> Word16
byteSwap32 :: Word32 -> Word32
byteSwap64 :: Word64 -> Word64

getWord8 :: Get Word8
putWord8 :: Word8 -> Put


class Binary t where
    put :: t -> Put
    default put :: (GHC.Generics.Generic t,
                    GBinaryPut (GHC.Generics.Rep t)) => t -> Put
    get :: Get t
    default get :: (GHC.Generics.Generic t,
                    GBinaryGet (GHC.Generics.Rep t)) => Get t
    putList :: [t] -> Put


class GBinaryGet (f :: k -> *) where
    gget :: forall (t :: k). Get (f t)
{-# MINIMAL gget #-}


class GBinaryPut (f :: k -> *) where
    gput :: forall (t :: k). f t -> Put
{-# MINIMAL gput #-}
```
