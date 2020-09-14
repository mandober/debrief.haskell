# Data.Word

Sized unsigned integral types:
- Word
- Word8
- Word16
- Word32
- Word64


Data.Word
data     Word        = GHC.Types.W#  GHC.Prim.Word#
data GHC.Word.Word8  = GHC.Word.W8#  GHC.Prim.Word#
data GHC.Word.Word16 = GHC.Word.W16# GHC.Prim.Word#
data GHC.Word.Word32 = GHC.Word.W32# GHC.Prim.Word#
data GHC.Word.Word64 = GHC.Word.W64# GHC.Prim.Word#
GHC.Word.byteSwap16 :: GHC.Word.Word16 -> GHC.Word.Word16
GHC.Word.byteSwap32 :: GHC.Word.Word32 -> GHC.Word.Word32
GHC.Word.byteSwap64 :: GHC.Word.Word64 -> GHC.Word.Word64
