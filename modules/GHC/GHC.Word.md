# GHC.Word

```hs
data GHC.Word.Word8  = ...ETC
data GHC.Word.Word16 = ...ETC
data GHC.Word.Word32 = ...ETC
data GHC.Word.Word64 = ...ETC

GHC.Word.W8#  :: GHC.Prim.Word# -> GHC.Word.Word8
GHC.Word.W16# :: GHC.Prim.Word# -> GHC.Word.Word16
GHC.Word.W32# :: GHC.Prim.Word# -> GHC.Word.Word32
GHC.Word.W64# :: GHC.Prim.Word# -> GHC.Word.Word64

GHC.Word.byteSwap16 :: GHC.Word.Word16 -> GHC.Word.Word16
GHC.Word.byteSwap32 :: GHC.Word.Word32 -> GHC.Word.Word32
GHC.Word.byteSwap64 :: GHC.Word.Word64 -> GHC.Word.Word64

GHC.Word.eqWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.eqWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.eqWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.eqWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.geWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.geWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.geWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.geWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.gtWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.gtWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.gtWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.gtWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.leWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.leWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.leWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.leWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.ltWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.ltWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.ltWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.ltWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.neWord8  :: GHC.Word.Word8  -> GHC.Word.Word8  -> Bool
GHC.Word.neWord16 :: GHC.Word.Word16 -> GHC.Word.Word16 -> Bool
GHC.Word.neWord32 :: GHC.Word.Word32 -> GHC.Word.Word32 -> Bool
GHC.Word.neWord64 :: GHC.Word.Word64 -> GHC.Word.Word64 -> Bool

GHC.Word.uncheckedShiftL64# :: GHC.Prim.Word# -> GHC.Prim.Int# -> GHC.Prim.Word#
GHC.Word.uncheckedShiftRL64#:: GHC.Prim.Word# -> GHC.Prim.Int# -> GHC.Prim.Word#

GHC.Types.W# :: GHC.Prim.Word# -> Word

GHC.Classes.eqWord :: Word -> Word -> Bool
GHC.Classes.neWord :: Word -> Word -> Bool

GHC.Classes.geWord :: Word -> Word -> Bool
GHC.Classes.gtWord :: Word -> Word -> Bool

GHC.Classes.leWord :: Word -> Word -> Bool
GHC.Classes.ltWord :: Word -> Word -> Bool

-- imported via Prelude
data Word = ...
```
