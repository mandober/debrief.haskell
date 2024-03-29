# GHC.Prim

https://hackage.haskell.org/package/ghc-prim-0.7.0/docs/GHC-Prim.html

GHC's primitive types and operations. Use [GHC.Exts](GHC.Exts.md) from the base package instead of importing this module directly.


```hs
-- import GHC.Prim   -- no
import GHC.Exts      -- use this

(*#)   :: Int# -> Int# -> Int#
(*##)  :: Double# -> Double# -> Double#
(**##) :: Double# -> Double# -> Double#
(+#)   :: Int# -> Int# -> Int#
(+##)  :: Double# -> Double# -> Double#
(-#)   :: Int# -> Int# -> Int#
(-##)  :: Double# -> Double# -> Double#
data (->) (a :: TYPE q) (b :: TYPE r)
(/##)  :: Double# -> Double# -> Double#
(/=#)  :: Int# -> Int# -> Int#
(/=##) :: Double# -> Double# -> Int#
(<#)   :: Int# -> Int# -> Int#
(<##)  :: Double# -> Double# -> Int#
(<=#)  :: Int# -> Int# -> Int#
(<=##) :: Double# -> Double# -> Int#
(==#)  :: Int# -> Int# -> Int#
(==##) :: Double# -> Double# -> Int#
(>#)   :: Int# -> Int# -> Int#
(>##)  :: Double# -> Double# -> Int#
(>=#)  :: Int# -> Int# -> Int#
(>=##) :: Double# -> Double# -> Int#


data Addr# :: TYPE 'GHC.Types.AddrRep
data Array# a :: TYPE 'GHC.Types.UnliftedRep
data ArrayArray# :: TYPE 'GHC.Types.UnliftedRep
data BCO# :: TYPE 'GHC.Types.UnliftedRep
data ByteArray# :: TYPE 'GHC.Types.UnliftedRep
data Char# :: TYPE 'GHC.Types.WordRep
data Compact# :: TYPE 'GHC.Types.UnliftedRep
data Double# :: TYPE 'GHC.Types.DoubleRep
data DoubleX2# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec2 'GHC.Types.DoubleElemRep)
data DoubleX4# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.DoubleElemRep)
data DoubleX8# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.DoubleElemRep)
data Float# :   : TYPE 'GHC.Types.FloatRep
data FloatX16# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.FloatElemRep)
data FloatX4# : : TYPE ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.FloatElemRep)
data FloatX8# : : TYPE ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.FloatElemRep)
data Int# :     : TYPE 'GHC.Types.IntRep
data Int16# :: TYPE 'GHC.Types.Int16Rep
data Int16X16# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Int16ElemRep)
data Int16X32# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec32 'GHC.Types.Int16ElemRep)
data Int16X8# : : TYPE ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Int16ElemRep)
data Int32# :   : TYPE 'GHC.Types.IntRep
data Int32X16# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Int32ElemRep)
data Int32X4# : : TYPE ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.Int32ElemRep)
data Int32X8# : : TYPE ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Int32ElemRep)
data Int64# :   : TYPE 'GHC.Types.Int64Rep
data Int64X2# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec2 'GHC.Types.Int64ElemRep)
data Int64X4# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.Int64ElemRep)
data Int64X8# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Int64ElemRep)
data Int8# :   : TYPE 'GHC.Types.Int8Rep
data Int8X16# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Int8ElemRep)
data Int8X32# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec32 'GHC.Types.Int8ElemRep)
data Int8X64# :: TYPE ('GHC.Types.VecRep 'GHC.Types.Vec64 'GHC.Types.Int8ElemRep)

type role MVar# nominal representational
data MVar# a b :: TYPE 'GHC.Types.UnliftedRep
type role MutVar# nominal representational
data MutVar# a b :: TYPE 'GHC.Types.UnliftedRep
type role MutableArray# nominal representational
data MutableArray# a b :: TYPE 'GHC.Types.UnliftedRep
type role MutableArrayArray# nominal
data MutableArrayArray# a :: TYPE 'GHC.Types.UnliftedRep
type role MutableByteArray# nominal
data MutableByteArray# a :: TYPE 'GHC.Types.UnliftedRep
type role Proxy# nominal
data Proxy# (a :: k) :: TYPE ('GHC.Types.TupleRep '[])
data RealWorld
data SmallArray# a :: TYPE 'GHC.Types.UnliftedRep
type role SmallMutableArray# nominal representational
data SmallMutableArray# a b :: TYPE 'GHC.Types.UnliftedRep
type role StableName# phantom
data StableName# a :: TYPE 'GHC.Types.UnliftedRep
data StablePtr# a :: TYPE 'GHC.Types.AddrRep
type role State# nominal
data State# a :: TYPE ('GHC.Types.TupleRep '[])
type role TVar# nominal representational
data TVar# a b :: TYPE 'GHC.Types.UnliftedRep
type role TYPE nominal
data TYPE (a :: GHC.Types.RuntimeRep)
data ThreadId# :: TYPE 'GHC.Types.UnliftedRep
data Void# :: TYPE ('GHC.Types.TupleRep '[])
data Weak# a :: TYPE 'GHC.Types.UnliftedRep
data Word# :: TYPE 'GHC.Types.WordRep
data Word16# :: TYPE 'GHC.Types.Word16Rep
data Word16X16# :: TYPE
                     ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Word16ElemRep)
data Word16X32# :: TYPE
                     ('GHC.Types.VecRep 'GHC.Types.Vec32 'GHC.Types.Word16ElemRep)
data Word16X8# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Word16ElemRep)
data Word32# :: TYPE 'GHC.Types.WordRep
data Word32X16# :: TYPE
                     ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Word32ElemRep)
data Word32X4# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.Word32ElemRep)
data Word32X8# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Word32ElemRep)
data Word64# :: TYPE 'GHC.Types.Word64Rep
data Word64X2# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec2 'GHC.Types.Word64ElemRep)
data Word64X4# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec4 'GHC.Types.Word64ElemRep)
data Word64X8# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec8 'GHC.Types.Word64ElemRep)
data Word8# :: TYPE 'GHC.Types.Word8Rep
data Word8X16# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec16 'GHC.Types.Word8ElemRep)
data Word8X32# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec32 'GHC.Types.Word8ElemRep)
data Word8X64# :: TYPE
                    ('GHC.Types.VecRep 'GHC.Types.Vec64 'GHC.Types.Word8ElemRep)


acosDouble# :: Double# -> Double#
acosFloat# :: Float# -> Float#
acoshDouble# :: Double# -> Double#
acoshFloat# :: Float# -> Float#

addCFinalizerToWeak# ::
  Addr#
  -> Addr#
  -> Int#
  -> Addr#
  -> Weak# b
  -> State# RealWorld
  -> (# State# RealWorld, Int# #)

addIntC# :: Int# -> Int# -> (# Int#, Int# #)
addWordC# :: Word# -> Word# -> (# Word#, Int# #)
addr2Int# :: Addr# -> Int#
addrToAny# :: Addr# -> (# a #)
and# :: Word# -> Word# -> Word#
andI# :: Int# -> Int# -> Int#
anyToAddr# ::
  a -> State# RealWorld -> (# State# RealWorld, Addr# #)
asinDouble# :: Double# -> Double#
asinFloat# :: Float# -> Float#
asinhDouble# :: Double# -> Double#
asinhFloat# :: Float# -> Float#
atanDouble# :: Double# -> Double#
atanFloat# :: Float# -> Float#
atanhDouble# :: Double# -> Double#
atanhFloat# :: Float# -> Float#
atomicModifyMutVar2# ::
  MutVar# d a -> (a -> c) -> State# d -> (# State# d, a, c #)
atomicModifyMutVar_# ::
  MutVar# d a -> (a -> a) -> State# d -> (# State# d, a, a #)
atomicReadIntArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
atomicWriteIntArray# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
atomically# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)
broadcastDoubleX2# :: Double# -> DoubleX2#
broadcastDoubleX4# :: Double# -> DoubleX4#
broadcastDoubleX8# :: Double# -> DoubleX8#
broadcastFloatX16# :: Float# -> FloatX16#
broadcastFloatX4# :: Float# -> FloatX4#
broadcastFloatX8# :: Float# -> FloatX8#
broadcastInt16X16# :: Int# -> Int16X16#
broadcastInt16X32# :: Int# -> Int16X32#
broadcastInt16X8# :: Int# -> Int16X8#
broadcastInt32X16# :: Int# -> Int32X16#
broadcastInt32X4# :: Int# -> Int32X4#
broadcastInt32X8# :: Int# -> Int32X8#
broadcastInt64X2# :: Int# -> Int64X2#
broadcastInt64X4# :: Int# -> Int64X4#
broadcastInt64X8# :: Int# -> Int64X8#
broadcastInt8X16# :: Int# -> Int8X16#
broadcastInt8X32# :: Int# -> Int8X32#
broadcastInt8X64# :: Int# -> Int8X64#
broadcastWord16X16# :: Word# -> Word16X16#
broadcastWord16X32# :: Word# -> Word16X32#
broadcastWord16X8# :: Word# -> Word16X8#
broadcastWord32X16# :: Word# -> Word32X16#
broadcastWord32X4# :: Word# -> Word32X4#
broadcastWord32X8# :: Word# -> Word32X8#
broadcastWord64X2# :: Word# -> Word64X2#
broadcastWord64X4# :: Word# -> Word64X4#
broadcastWord64X8# :: Word# -> Word64X8#
broadcastWord8X16# :: Word# -> Word8X16#
broadcastWord8X32# :: Word# -> Word8X32#
broadcastWord8X64# :: Word# -> Word8X64#
byteArrayContents# :: ByteArray# -> Addr#
byteSwap# :: Word# -> Word#
byteSwap16# :: Word# -> Word#
byteSwap32# :: Word# -> Word#
byteSwap64# :: Word# -> Word#
casArray# ::
  MutableArray# d a
  -> Int# -> a -> a -> State# d -> (# State# d, Int#, a #)
casIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> Int# -> State# d -> (# State# d, Int# #)
casMutVar# ::
  MutVar# d a -> a -> a -> State# d -> (# State# d, Int#, a #)
casSmallArray# ::
  SmallMutableArray# d a
  -> Int# -> a -> a -> State# d -> (# State# d, Int#, a #)
catch# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> (b -> State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld
  -> (# State# RealWorld, a #)
catchRetry# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld
  -> (# State# RealWorld, a #)
catchSTM# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> (b -> State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld
  -> (# State# RealWorld, a #)
chr# :: Int# -> Char#
clearCCS# ::
  (State# d -> (# State# d, a #)) -> State# d -> (# State# d, a #)
cloneArray# :: Array# a -> Int# -> Int# -> Array# a
cloneMutableArray# ::
  MutableArray# d a
  -> Int# -> Int# -> State# d -> (# State# d, MutableArray# d a #)
cloneSmallArray# :: SmallArray# a -> Int# -> Int# -> SmallArray# a
cloneSmallMutableArray# ::
  SmallMutableArray# d a
  -> Int#
  -> Int#
  -> State# d
  -> (# State# d, SmallMutableArray# d a #)
clz# :: Word# -> Word#
clz16# :: Word# -> Word#
clz32# :: Word# -> Word#
clz64# :: Word# -> Word#
clz8# :: Word# -> Word#
coerce :: Coercible a b => a -> b
compactAdd# ::
  Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
compactAddWithSharing# ::
  Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
compactAllocateBlock# ::
  Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
compactContains# ::
  Compact# -> a -> State# RealWorld -> (# State# RealWorld, Int# #)
compactContainsAny# ::
  a -> State# RealWorld -> (# State# RealWorld, Int# #)
compactFixupPointers# ::
  Addr#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Compact#, Addr# #)
compactGetFirstBlock# ::
  Compact#
  -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
compactGetNextBlock# ::
  Compact#
  -> Addr#
  -> State# RealWorld
  -> (# State# RealWorld, Addr#, Word# #)
compactNew# ::
  Word# -> State# RealWorld -> (# State# RealWorld, Compact# #)
compactResize# ::
  Compact# -> Word# -> State# RealWorld -> State# RealWorld
compactSize# ::
  Compact# -> State# RealWorld -> (# State# RealWorld, Word# #)
compareByteArrays# ::
  ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
copyAddrToByteArray# ::
  Addr#
  -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
copyArray# ::
  Array# a
  -> Int#
  -> MutableArray# d a
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyArrayArray# ::
  ArrayArray#
  -> Int#
  -> MutableArrayArray# d
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyByteArray# ::
  ByteArray#
  -> Int#
  -> MutableByteArray# d
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyByteArrayToAddr# ::
  ByteArray# -> Int# -> Addr# -> Int# -> State# d -> State# d
copyMutableArray# ::
  MutableArray# d a
  -> Int#
  -> MutableArray# d a
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyMutableArrayArray# ::
  MutableArrayArray# d
  -> Int#
  -> MutableArrayArray# d
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyMutableByteArray# ::
  MutableByteArray# d
  -> Int#
  -> MutableByteArray# d
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copyMutableByteArrayToAddr# ::
  MutableByteArray# d
  -> Int# -> Addr# -> Int# -> State# d -> State# d
copySmallArray# ::
  SmallArray# a
  -> Int#
  -> SmallMutableArray# d a
  -> Int#
  -> Int#
  -> State# d
  -> State# d
copySmallMutableArray# ::
  SmallMutableArray# d a
  -> Int#
  -> SmallMutableArray# d a
  -> Int#
  -> Int#
  -> State# d
  -> State# d
cosDouble# :: Double# -> Double#
cosFloat# :: Float# -> Float#
coshDouble# :: Double# -> Double#
coshFloat# :: Float# -> Float#
ctz# :: Word# -> Word#
ctz16# :: Word# -> Word#
ctz32# :: Word# -> Word#
ctz64# :: Word# -> Word#
ctz8# :: Word# -> Word#
dataToTag# :: a -> Int#
deRefStablePtr# ::
  StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
deRefWeak# ::
  Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
decodeDouble_2Int# :: Double# -> (# Int#, Word#, Word#, Int# #)
decodeDouble_Int64# :: Double# -> (# Int#, Int# #)
decodeFloat_Int# :: Float# -> (# Int#, Int# #)
delay# :: Int# -> State# d -> State# d
divideDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#
divideDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
divideDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#
divideFloat# :: Float# -> Float# -> Float#
divideFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#
divideFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
divideFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#
double2Float# :: Double# -> Float#
double2Int# :: Double# -> Int#
eqAddr# :: Addr# -> Addr# -> Int#
eqChar# :: Char# -> Char# -> Int#
eqFloat# :: Float# -> Float# -> Int#
eqInt16# :: Int16# -> Int16# -> Int#
eqInt8# :: Int8# -> Int8# -> Int#
eqStableName# :: StableName# a -> StableName# b -> Int#
eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
eqWord# :: Word# -> Word# -> Int#
eqWord16# :: Word16# -> Word16# -> Int#
eqWord8# :: Word8# -> Word8# -> Int#
expDouble# :: Double# -> Double#
expFloat# :: Float# -> Float#
extendInt16# :: Int16# -> Int#
extendInt8# :: Int8# -> Int#
extendWord16# :: Word16# -> Word#
extendWord8# :: Word8# -> Word#
fabsDouble# :: Double# -> Double#
fabsFloat# :: Float# -> Float#
fetchAddIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
fetchAndIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
fetchNandIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
fetchOrIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
fetchSubIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
fetchXorIntArray# ::
  MutableByteArray# d
  -> Int# -> Int# -> State# d -> (# State# d, Int# #)
finalizeWeak# ::
  Weak# a
  -> State# RealWorld
  -> (# State# RealWorld, Int#,
        State# RealWorld -> (# State# RealWorld, b #) #)
float2Double# :: Float# -> Double#
float2Int# :: Float# -> Int#
fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
forkOn# ::
  Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
freezeArray# ::
  MutableArray# d a
  -> Int# -> Int# -> State# d -> (# State# d, Array# a #)
freezeSmallArray# ::
  SmallMutableArray# d a
  -> Int# -> Int# -> State# d -> (# State# d, SmallArray# a #)
geAddr# :: Addr# -> Addr# -> Int#
geChar# :: Char# -> Char# -> Int#
geFloat# :: Float# -> Float# -> Int#
geInt16# :: Int16# -> Int16# -> Int#
geInt8# :: Int8# -> Int8# -> Int#
geWord# :: Word# -> Word# -> Int#
geWord16# :: Word16# -> Word16# -> Int#
geWord8# :: Word8# -> Word8# -> Int#
getApStackVal# :: a -> Int# -> (# Int#, b #)
getCCSOf# :: a -> State# d -> (# State# d, Addr# #)
getCurrentCCS# :: a -> State# d -> (# State# d, Addr# #)
getMaskingState# ::
  State# RealWorld -> (# State# RealWorld, Int# #)
getSizeofMutableByteArray# ::
  MutableByteArray# d -> State# d -> (# State# d, Int# #)
getSpark# :: State# d -> (# State# d, Int#, a #)
getThreadAllocationCounter# ::
  State# RealWorld -> (# State# RealWorld, Int# #)
gtAddr# :: Addr# -> Addr# -> Int#
gtChar# :: Char# -> Char# -> Int#
gtFloat# :: Float# -> Float# -> Int#
gtInt16# :: Int16# -> Int16# -> Int#
gtInt8# :: Int8# -> Int8# -> Int#
gtWord# :: Word# -> Word# -> Int#
gtWord16# :: Word16# -> Word16# -> Int#
gtWord8# :: Word8# -> Word8# -> Int#
indexAddrArray# :: ByteArray# -> Int# -> Addr#
indexAddrOffAddr# :: Addr# -> Int# -> Addr#
indexArray# :: Array# a -> Int# -> (# a #)
indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
indexCharArray# :: ByteArray# -> Int# -> Char#
indexCharOffAddr# :: Addr# -> Int# -> Char#
indexDoubleArray# :: ByteArray# -> Int# -> Double#
indexDoubleArrayAsDoubleX2# :: ByteArray# -> Int# -> DoubleX2#
indexDoubleArrayAsDoubleX4# :: ByteArray# -> Int# -> DoubleX4#
indexDoubleArrayAsDoubleX8# :: ByteArray# -> Int# -> DoubleX8#
indexDoubleOffAddr# :: Addr# -> Int# -> Double#
indexDoubleOffAddrAsDoubleX2# :: Addr# -> Int# -> DoubleX2#
indexDoubleOffAddrAsDoubleX4# :: Addr# -> Int# -> DoubleX4#
indexDoubleOffAddrAsDoubleX8# :: Addr# -> Int# -> DoubleX8#
indexDoubleX2Array# :: ByteArray# -> Int# -> DoubleX2#
indexDoubleX2OffAddr# :: Addr# -> Int# -> DoubleX2#
indexDoubleX4Array# :: ByteArray# -> Int# -> DoubleX4#
indexDoubleX4OffAddr# :: Addr# -> Int# -> DoubleX4#
indexDoubleX8Array# :: ByteArray# -> Int# -> DoubleX8#
indexDoubleX8OffAddr# :: Addr# -> Int# -> DoubleX8#
indexFloatArray# :: ByteArray# -> Int# -> Float#
indexFloatArrayAsFloatX16# :: ByteArray# -> Int# -> FloatX16#
indexFloatArrayAsFloatX4# :: ByteArray# -> Int# -> FloatX4#
indexFloatArrayAsFloatX8# :: ByteArray# -> Int# -> FloatX8#
indexFloatOffAddr# :: Addr# -> Int# -> Float#
indexFloatOffAddrAsFloatX16# :: Addr# -> Int# -> FloatX16#
indexFloatOffAddrAsFloatX4# :: Addr# -> Int# -> FloatX4#
indexFloatOffAddrAsFloatX8# :: Addr# -> Int# -> FloatX8#
indexFloatX16Array# :: ByteArray# -> Int# -> FloatX16#
indexFloatX16OffAddr# :: Addr# -> Int# -> FloatX16#
indexFloatX4Array# :: ByteArray# -> Int# -> FloatX4#
indexFloatX4OffAddr# :: Addr# -> Int# -> FloatX4#
indexFloatX8Array# :: ByteArray# -> Int# -> FloatX8#
indexFloatX8OffAddr# :: Addr# -> Int# -> FloatX8#
indexInt16Array# :: ByteArray# -> Int# -> Int#
indexInt16ArrayAsInt16X16# :: ByteArray# -> Int# -> Int16X16#
indexInt16ArrayAsInt16X32# :: ByteArray# -> Int# -> Int16X32#
indexInt16ArrayAsInt16X8# :: ByteArray# -> Int# -> Int16X8#
indexInt16OffAddr# :: Addr# -> Int# -> Int#
indexInt16OffAddrAsInt16X16# :: Addr# -> Int# -> Int16X16#
indexInt16OffAddrAsInt16X32# :: Addr# -> Int# -> Int16X32#
indexInt16OffAddrAsInt16X8# :: Addr# -> Int# -> Int16X8#
indexInt16X16Array# :: ByteArray# -> Int# -> Int16X16#
indexInt16X16OffAddr# :: Addr# -> Int# -> Int16X16#
indexInt16X32Array# :: ByteArray# -> Int# -> Int16X32#
indexInt16X32OffAddr# :: Addr# -> Int# -> Int16X32#
indexInt16X8Array# :: ByteArray# -> Int# -> Int16X8#
indexInt16X8OffAddr# :: Addr# -> Int# -> Int16X8#
indexInt32Array# :: ByteArray# -> Int# -> Int#
indexInt32ArrayAsInt32X16# :: ByteArray# -> Int# -> Int32X16#
indexInt32ArrayAsInt32X4# :: ByteArray# -> Int# -> Int32X4#
indexInt32ArrayAsInt32X8# :: ByteArray# -> Int# -> Int32X8#
indexInt32OffAddr# :: Addr# -> Int# -> Int#
indexInt32OffAddrAsInt32X16# :: Addr# -> Int# -> Int32X16#
indexInt32OffAddrAsInt32X4# :: Addr# -> Int# -> Int32X4#
indexInt32OffAddrAsInt32X8# :: Addr# -> Int# -> Int32X8#
indexInt32X16Array# :: ByteArray# -> Int# -> Int32X16#
indexInt32X16OffAddr# :: Addr# -> Int# -> Int32X16#
indexInt32X4Array# :: ByteArray# -> Int# -> Int32X4#
indexInt32X4OffAddr# :: Addr# -> Int# -> Int32X4#
indexInt32X8Array# :: ByteArray# -> Int# -> Int32X8#
indexInt32X8OffAddr# :: Addr# -> Int# -> Int32X8#
indexInt64Array# :: ByteArray# -> Int# -> Int#
indexInt64ArrayAsInt64X2# :: ByteArray# -> Int# -> Int64X2#
indexInt64ArrayAsInt64X4# :: ByteArray# -> Int# -> Int64X4#
indexInt64ArrayAsInt64X8# :: ByteArray# -> Int# -> Int64X8#
indexInt64OffAddr# :: Addr# -> Int# -> Int#
indexInt64OffAddrAsInt64X2# :: Addr# -> Int# -> Int64X2#
indexInt64OffAddrAsInt64X4# :: Addr# -> Int# -> Int64X4#
indexInt64OffAddrAsInt64X8# :: Addr# -> Int# -> Int64X8#
indexInt64X2Array# :: ByteArray# -> Int# -> Int64X2#
indexInt64X2OffAddr# :: Addr# -> Int# -> Int64X2#
indexInt64X4Array# :: ByteArray# -> Int# -> Int64X4#
indexInt64X4OffAddr# :: Addr# -> Int# -> Int64X4#
indexInt64X8Array# :: ByteArray# -> Int# -> Int64X8#
indexInt64X8OffAddr# :: Addr# -> Int# -> Int64X8#
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt8ArrayAsInt8X16# :: ByteArray# -> Int# -> Int8X16#
indexInt8ArrayAsInt8X32# :: ByteArray# -> Int# -> Int8X32#
indexInt8ArrayAsInt8X64# :: ByteArray# -> Int# -> Int8X64#
indexInt8OffAddr# :: Addr# -> Int# -> Int#
indexInt8OffAddrAsInt8X16# :: Addr# -> Int# -> Int8X16#
indexInt8OffAddrAsInt8X32# :: Addr# -> Int# -> Int8X32#
indexInt8OffAddrAsInt8X64# :: Addr# -> Int# -> Int8X64#
indexInt8X16Array# :: ByteArray# -> Int# -> Int8X16#
indexInt8X16OffAddr# :: Addr# -> Int# -> Int8X16#
indexInt8X32Array# :: ByteArray# -> Int# -> Int8X32#
indexInt8X32OffAddr# :: Addr# -> Int# -> Int8X32#
indexInt8X64Array# :: ByteArray# -> Int# -> Int8X64#
indexInt8X64OffAddr# :: Addr# -> Int# -> Int8X64#
indexIntArray# :: ByteArray# -> Int# -> Int#
indexIntOffAddr# :: Addr# -> Int# -> Int#
indexSmallArray# :: SmallArray# a -> Int# -> (# a #)
indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a
indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
indexWideCharArray# :: ByteArray# -> Int# -> Char#
indexWideCharOffAddr# :: Addr# -> Int# -> Char#
indexWord16Array# :: ByteArray# -> Int# -> Word#
indexWord16ArrayAsWord16X16# :: ByteArray# -> Int# -> Word16X16#
indexWord16ArrayAsWord16X32# :: ByteArray# -> Int# -> Word16X32#
indexWord16ArrayAsWord16X8# :: ByteArray# -> Int# -> Word16X8#
indexWord16OffAddr# :: Addr# -> Int# -> Word#
indexWord16OffAddrAsWord16X16# :: Addr# -> Int# -> Word16X16#
indexWord16OffAddrAsWord16X32# :: Addr# -> Int# -> Word16X32#
indexWord16OffAddrAsWord16X8# :: Addr# -> Int# -> Word16X8#
indexWord16X16Array# :: ByteArray# -> Int# -> Word16X16#
indexWord16X16OffAddr# :: Addr# -> Int# -> Word16X16#
indexWord16X32Array# :: ByteArray# -> Int# -> Word16X32#
indexWord16X32OffAddr# :: Addr# -> Int# -> Word16X32#
indexWord16X8Array# :: ByteArray# -> Int# -> Word16X8#
indexWord16X8OffAddr# :: Addr# -> Int# -> Word16X8#
indexWord32Array# :: ByteArray# -> Int# -> Word#
indexWord32ArrayAsWord32X16# :: ByteArray# -> Int# -> Word32X16#
indexWord32ArrayAsWord32X4# :: ByteArray# -> Int# -> Word32X4#
indexWord32ArrayAsWord32X8# :: ByteArray# -> Int# -> Word32X8#
indexWord32OffAddr# :: Addr# -> Int# -> Word#
indexWord32OffAddrAsWord32X16# :: Addr# -> Int# -> Word32X16#
indexWord32OffAddrAsWord32X4# :: Addr# -> Int# -> Word32X4#
indexWord32OffAddrAsWord32X8# :: Addr# -> Int# -> Word32X8#
indexWord32X16Array# :: ByteArray# -> Int# -> Word32X16#
indexWord32X16OffAddr# :: Addr# -> Int# -> Word32X16#
indexWord32X4Array# :: ByteArray# -> Int# -> Word32X4#
indexWord32X4OffAddr# :: Addr# -> Int# -> Word32X4#
indexWord32X8Array# :: ByteArray# -> Int# -> Word32X8#
indexWord32X8OffAddr# :: Addr# -> Int# -> Word32X8#
indexWord64Array# :: ByteArray# -> Int# -> Word#
indexWord64ArrayAsWord64X2# :: ByteArray# -> Int# -> Word64X2#
indexWord64ArrayAsWord64X4# :: ByteArray# -> Int# -> Word64X4#
indexWord64ArrayAsWord64X8# :: ByteArray# -> Int# -> Word64X8#
indexWord64OffAddr# :: Addr# -> Int# -> Word#
indexWord64OffAddrAsWord64X2# :: Addr# -> Int# -> Word64X2#
indexWord64OffAddrAsWord64X4# :: Addr# -> Int# -> Word64X4#
indexWord64OffAddrAsWord64X8# :: Addr# -> Int# -> Word64X8#
indexWord64X2Array# :: ByteArray# -> Int# -> Word64X2#
indexWord64X2OffAddr# :: Addr# -> Int# -> Word64X2#
indexWord64X4Array# :: ByteArray# -> Int# -> Word64X4#
indexWord64X4OffAddr# :: Addr# -> Int# -> Word64X4#
indexWord64X8Array# :: ByteArray# -> Int# -> Word64X8#
indexWord64X8OffAddr# :: Addr# -> Int# -> Word64X8#
indexWord8Array# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#
indexWord8ArrayAsChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#
indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#
indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a
indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord8X16# :: ByteArray# -> Int# -> Word8X16#
indexWord8ArrayAsWord8X32# :: ByteArray# -> Int# -> Word8X32#
indexWord8ArrayAsWord8X64# :: ByteArray# -> Int# -> Word8X64#
indexWord8OffAddr# :: Addr# -> Int# -> Word#
indexWord8OffAddrAsWord8X16# :: Addr# -> Int# -> Word8X16#
indexWord8OffAddrAsWord8X32# :: Addr# -> Int# -> Word8X32#
indexWord8OffAddrAsWord8X64# :: Addr# -> Int# -> Word8X64#
indexWord8X16Array# :: ByteArray# -> Int# -> Word8X16#
indexWord8X16OffAddr# :: Addr# -> Int# -> Word8X16#
indexWord8X32Array# :: ByteArray# -> Int# -> Word8X32#
indexWord8X32OffAddr# :: Addr# -> Int# -> Word8X32#
indexWord8X64Array# :: ByteArray# -> Int# -> Word8X64#
indexWord8X64OffAddr# :: Addr# -> Int# -> Word8X64#
indexWordArray# :: ByteArray# -> Int# -> Word#
indexWordOffAddr# :: Addr# -> Int# -> Word#
insertDoubleX2# :: DoubleX2# -> Double# -> Int# -> DoubleX2#
insertDoubleX4# :: DoubleX4# -> Double# -> Int# -> DoubleX4#
insertDoubleX8# :: DoubleX8# -> Double# -> Int# -> DoubleX8#
insertFloatX16# :: FloatX16# -> Float# -> Int# -> FloatX16#
insertFloatX4# :: FloatX4# -> Float# -> Int# -> FloatX4#
insertFloatX8# :: FloatX8# -> Float# -> Int# -> FloatX8#
insertInt16X16# :: Int16X16# -> Int# -> Int# -> Int16X16#
insertInt16X32# :: Int16X32# -> Int# -> Int# -> Int16X32#
insertInt16X8# :: Int16X8# -> Int# -> Int# -> Int16X8#
insertInt32X16# :: Int32X16# -> Int# -> Int# -> Int32X16#
insertInt32X4# :: Int32X4# -> Int# -> Int# -> Int32X4#
insertInt32X8# :: Int32X8# -> Int# -> Int# -> Int32X8#
insertInt64X2# :: Int64X2# -> Int# -> Int# -> Int64X2#
insertInt64X4# :: Int64X4# -> Int# -> Int# -> Int64X4#
insertInt64X8# :: Int64X8# -> Int# -> Int# -> Int64X8#
insertInt8X16# :: Int8X16# -> Int# -> Int# -> Int8X16#
insertInt8X32# :: Int8X32# -> Int# -> Int# -> Int8X32#
insertInt8X64# :: Int8X64# -> Int# -> Int# -> Int8X64#
insertWord16X16# :: Word16X16# -> Word# -> Int# -> Word16X16#
insertWord16X32# :: Word16X32# -> Word# -> Int# -> Word16X32#
insertWord16X8# :: Word16X8# -> Word# -> Int# -> Word16X8#
insertWord32X16# :: Word32X16# -> Word# -> Int# -> Word32X16#
insertWord32X4# :: Word32X4# -> Word# -> Int# -> Word32X4#
insertWord32X8# :: Word32X8# -> Word# -> Int# -> Word32X8#
insertWord64X2# :: Word64X2# -> Word# -> Int# -> Word64X2#
insertWord64X4# :: Word64X4# -> Word# -> Int# -> Word64X4#
insertWord64X8# :: Word64X8# -> Word# -> Int# -> Word64X8#
insertWord8X16# :: Word8X16# -> Word# -> Int# -> Word8X16#
insertWord8X32# :: Word8X32# -> Word# -> Int# -> Word8X32#
insertWord8X64# :: Word8X64# -> Word# -> Int# -> Word8X64#
int2Addr# :: Int# -> Addr#
int2Double# :: Int# -> Double#
int2Float# :: Int# -> Float#
int2Word# :: Int# -> Word#
isByteArrayPinned# :: ByteArray# -> Int#
isCurrentThreadBound# ::
  State# RealWorld -> (# State# RealWorld, Int# #)
isEmptyMVar# :: MVar# d a -> State# d -> (# State# d, Int# #)
isMutableByteArrayPinned# :: MutableByteArray# d -> Int#
killThread# ::
  ThreadId# -> a -> State# RealWorld -> State# RealWorld
labelThread# ::
  ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
leAddr# :: Addr# -> Addr# -> Int#
leChar# :: Char# -> Char# -> Int#
leFloat# :: Float# -> Float# -> Int#
leInt16# :: Int16# -> Int16# -> Int#
leInt8# :: Int8# -> Int8# -> Int#
leWord# :: Word# -> Word# -> Int#
leWord16# :: Word16# -> Word16# -> Int#
leWord8# :: Word8# -> Word8# -> Int#
logDouble# :: Double# -> Double#
logFloat# :: Float# -> Float#
ltAddr# :: Addr# -> Addr# -> Int#
ltChar# :: Char# -> Char# -> Int#
ltFloat# :: Float# -> Float# -> Int#
ltInt16# :: Int16# -> Int16# -> Int#
ltInt8# :: Int8# -> Int8# -> Int#
ltWord# :: Word# -> Word# -> Int#
ltWord16# :: Word16# -> Word16# -> Int#
ltWord8# :: Word8# -> Word8# -> Int#
magicDict :: a
makeStableName# ::
  a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
makeStablePtr# ::
  a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
maskAsyncExceptions# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)
maskUninterruptible# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)
minusAddr# :: Addr# -> Addr# -> Int#
minusDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#
minusDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
minusDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#
minusFloat# :: Float# -> Float# -> Float#
minusFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#
minusFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
minusFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#
minusInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
minusInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
minusInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
minusInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
minusInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
minusInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
minusInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
minusInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
minusInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
minusInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
minusInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
minusInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
minusWord# :: Word# -> Word# -> Word#
minusWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
minusWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
minusWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
minusWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
minusWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
minusWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
minusWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
minusWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
minusWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
minusWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
minusWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
minusWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
mkApUpd0# :: BCO# -> (# a #)
mkWeak# ::
  a
  -> b
  -> (State# RealWorld -> (# State# RealWorld, c #))
  -> State# RealWorld
  -> (# State# RealWorld, Weak# b #)
mkWeakNoFinalizer# ::
  a -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
mulIntMayOflo# :: Int# -> Int# -> Int#
myThreadId# ::
  State# RealWorld -> (# State# RealWorld, ThreadId# #)
narrow16Int# :: Int# -> Int#
narrow16Word# :: Word# -> Word#
narrow32Int# :: Int# -> Int#
narrow32Word# :: Word# -> Word#
narrow8Int# :: Int# -> Int#
narrow8Word# :: Word# -> Word#
narrowInt16# :: Int# -> Int16#
narrowInt8# :: Int# -> Int8#
narrowWord16# :: Word# -> Word16#
narrowWord8# :: Word# -> Word8#
neAddr# :: Addr# -> Addr# -> Int#
neChar# :: Char# -> Char# -> Int#
neFloat# :: Float# -> Float# -> Int#
neInt16# :: Int16# -> Int16# -> Int#
neInt8# :: Int8# -> Int8# -> Int#
neWord# :: Word# -> Word# -> Int#
neWord16# :: Word16# -> Word16# -> Int#
neWord8# :: Word8# -> Word8# -> Int#
negateDouble# :: Double# -> Double#
negateDoubleX2# :: DoubleX2# -> DoubleX2#
negateDoubleX4# :: DoubleX4# -> DoubleX4#
negateDoubleX8# :: DoubleX8# -> DoubleX8#
negateFloat# :: Float# -> Float#
negateFloatX16# :: FloatX16# -> FloatX16#
negateFloatX4# :: FloatX4# -> FloatX4#
negateFloatX8# :: FloatX8# -> FloatX8#
negateInt# :: Int# -> Int#
negateInt16# :: Int16# -> Int16#
negateInt16X16# :: Int16X16# -> Int16X16#
negateInt16X32# :: Int16X32# -> Int16X32#
negateInt16X8# :: Int16X8# -> Int16X8#
negateInt32X16# :: Int32X16# -> Int32X16#
negateInt32X4# :: Int32X4# -> Int32X4#
negateInt32X8# :: Int32X8# -> Int32X8#
negateInt64X2# :: Int64X2# -> Int64X2#
negateInt64X4# :: Int64X4# -> Int64X4#
negateInt64X8# :: Int64X8# -> Int64X8#
negateInt8# :: Int8# -> Int8#
negateInt8X16# :: Int8X16# -> Int8X16#
negateInt8X32# :: Int8X32# -> Int8X32#
negateInt8X64# :: Int8X64# -> Int8X64#
newAlignedPinnedByteArray# ::
  Int# -> Int# -> State# d -> (# State# d, MutableByteArray# d #)
newArray# ::
  Int# -> a -> State# d -> (# State# d, MutableArray# d a #)
newArrayArray# ::
  Int# -> State# d -> (# State# d, MutableArrayArray# d #)
newBCO# ::
  ByteArray#
  -> ByteArray#
  -> Array# a
  -> Int#
  -> ByteArray#
  -> State# d
  -> (# State# d, BCO# #)
newByteArray# ::
  Int# -> State# d -> (# State# d, MutableByteArray# d #)
newMVar# :: State# d -> (# State# d, MVar# d a #)
newMutVar# :: a -> State# d -> (# State# d, MutVar# d a #)
newPinnedByteArray# ::
  Int# -> State# d -> (# State# d, MutableByteArray# d #)
newSmallArray# ::
  Int# -> a -> State# d -> (# State# d, SmallMutableArray# d a #)
newTVar# :: a -> State# d -> (# State# d, TVar# d a #)
noDuplicate# :: State# d -> State# d
not# :: Word# -> Word#
notI# :: Int# -> Int#
notWord16# :: Word16# -> Word16#
notWord8# :: Word8# -> Word8#
nullAddr# :: Addr#
numSparks# :: State# d -> (# State# d, Int# #)
or# :: Word# -> Word# -> Word#
orI# :: Int# -> Int# -> Int#
ord# :: Char# -> Int#
packDoubleX2# :: (# Double#, Double# #) -> DoubleX2#
packDoubleX4# ::
  (# Double#, Double#, Double#, Double# #) -> DoubleX4#
packDoubleX8# ::
  (# Double#, Double#, Double#, Double#, Double#, Double#, Double#,
     Double# #)
  -> DoubleX8#
packFloatX16# ::
  (# Float#, Float#, Float#, Float#, Float#, Float#, Float#, Float#,
     Float#, Float#, Float#, Float#, Float#, Float#, Float#, Float# #)
  -> FloatX16#
packFloatX4# :: (# Float#, Float#, Float#, Float# #) -> FloatX4#
packFloatX8# ::
  (# Float#, Float#, Float#, Float#, Float#, Float#, Float#,
     Float# #)
  -> FloatX8#
packInt16X16# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int16X16#
packInt16X32# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int16X32#
packInt16X8# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #) -> Int16X8#
packInt32X16# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int32X16#
packInt32X4# :: (# Int#, Int#, Int#, Int# #) -> Int32X4#
packInt32X8# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #) -> Int32X8#
packInt64X2# :: (# Int#, Int# #) -> Int64X2#
packInt64X4# :: (# Int#, Int#, Int#, Int# #) -> Int64X4#
packInt64X8# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #) -> Int64X8#
packInt8X16# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int8X16#
packInt8X32# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int8X32#
packInt8X64# ::
  (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
     Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
  -> Int8X64#
packWord16X16# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word16X16#
packWord16X32# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word# #)
  -> Word16X32#
packWord16X8# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word16X8#
packWord32X16# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word32X16#
packWord32X4# :: (# Word#, Word#, Word#, Word# #) -> Word32X4#
packWord32X8# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word32X8#
packWord64X2# :: (# Word#, Word# #) -> Word64X2#
packWord64X4# :: (# Word#, Word#, Word#, Word# #) -> Word64X4#
packWord64X8# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word64X8#
packWord8X16# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
  -> Word8X16#
packWord8X32# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word# #)
  -> Word8X32#
packWord8X64# ::
  (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
     Word# #)
  -> Word8X64#
par# :: a -> Int#
pdep# :: Word# -> Word# -> Word#
pdep16# :: Word# -> Word# -> Word#
pdep32# :: Word# -> Word# -> Word#
pdep64# :: Word# -> Word# -> Word#
pdep8# :: Word# -> Word# -> Word#
pext# :: Word# -> Word# -> Word#
pext16# :: Word# -> Word# -> Word#
pext32# :: Word# -> Word# -> Word#
pext64# :: Word# -> Word# -> Word#
pext8# :: Word# -> Word# -> Word#
plusAddr# :: Addr# -> Int# -> Addr#
plusDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#
plusDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
plusDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#
plusFloat# :: Float# -> Float# -> Float#
plusFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#
plusFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
plusFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#
plusInt16# :: Int16# -> Int16# -> Int16#
plusInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
plusInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
plusInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
plusInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
plusInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
plusInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
plusInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
plusInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
plusInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
plusInt8# :: Int8# -> Int8# -> Int8#
plusInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
plusInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
plusInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
plusWord# :: Word# -> Word# -> Word#
plusWord16# :: Word16# -> Word16# -> Word16#
plusWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
plusWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
plusWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
plusWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
plusWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
plusWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
plusWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
plusWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
plusWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
plusWord8# :: Word8# -> Word8# -> Word8#
plusWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
plusWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
plusWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
popCnt# :: Word# -> Word#
popCnt16# :: Word# -> Word#
popCnt32# :: Word# -> Word#
popCnt64# :: Word# -> Word#
popCnt8# :: Word# -> Word#
powerFloat# :: Float# -> Float# -> Float#
prefetchAddr0# :: Addr# -> Int# -> State# d -> State# d
prefetchAddr1# :: Addr# -> Int# -> State# d -> State# d
prefetchAddr2# :: Addr# -> Int# -> State# d -> State# d
prefetchAddr3# :: Addr# -> Int# -> State# d -> State# d
prefetchByteArray0# :: ByteArray# -> Int# -> State# d -> State# d
prefetchByteArray1# :: ByteArray# -> Int# -> State# d -> State# d
prefetchByteArray2# :: ByteArray# -> Int# -> State# d -> State# d
prefetchByteArray3# :: ByteArray# -> Int# -> State# d -> State# d
prefetchMutableByteArray0# ::
  MutableByteArray# d -> Int# -> State# d -> State# d
prefetchMutableByteArray1# ::
  MutableByteArray# d -> Int# -> State# d -> State# d
prefetchMutableByteArray2# ::
  MutableByteArray# d -> Int# -> State# d -> State# d
prefetchMutableByteArray3# ::
  MutableByteArray# d -> Int# -> State# d -> State# d
prefetchValue0# :: a -> State# d -> State# d
prefetchValue1# :: a -> State# d -> State# d
prefetchValue2# :: a -> State# d -> State# d
prefetchValue3# :: a -> State# d -> State# d
proxy# :: forall k (a :: k). Proxy# a
putMVar# :: MVar# d a -> a -> State# d -> State# d
quotInt# :: Int# -> Int# -> Int#
quotInt16# :: Int16# -> Int16# -> Int16#
quotInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
quotInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
quotInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
quotInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
quotInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
quotInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
quotInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
quotInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
quotInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
quotInt8# :: Int8# -> Int8# -> Int8#
quotInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
quotInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
quotInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
quotRemInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)
quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
quotRemWord8# :: Word8# -> Word8# -> (# Word8#, Word8# #)
quotWord# :: Word# -> Word# -> Word#
quotWord16# :: Word16# -> Word16# -> Word16#
quotWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
quotWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
quotWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
quotWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
quotWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
quotWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
quotWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
quotWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
quotWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
quotWord8# :: Word8# -> Word8# -> Word8#
quotWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
quotWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
quotWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
raise# :: b -> a
raiseIO# :: a -> State# RealWorld -> (# State# RealWorld, b #)
readAddrArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)
readAddrOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Addr# #)
readArray# ::
  MutableArray# d a -> Int# -> State# d -> (# State# d, a #)
readArrayArrayArray# ::
  MutableArrayArray# d
  -> Int# -> State# d -> (# State# d, ArrayArray# #)
readByteArrayArray# ::
  MutableArrayArray# d
  -> Int# -> State# d -> (# State# d, ByteArray# #)
readCharArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readCharOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Char# #)
readDoubleArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readDoubleArrayAsDoubleX2# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX2# #)
readDoubleArrayAsDoubleX4# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX4# #)
readDoubleArrayAsDoubleX8# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX8# #)
readDoubleOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Double# #)
readDoubleOffAddrAsDoubleX2# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX2# #)
readDoubleOffAddrAsDoubleX4# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX4# #)
readDoubleOffAddrAsDoubleX8# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX8# #)
readDoubleX2Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX2# #)
readDoubleX2OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX2# #)
readDoubleX4Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX4# #)
readDoubleX4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX4# #)
readDoubleX8Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, DoubleX8# #)
readDoubleX8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, DoubleX8# #)
readFloatArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)
readFloatArrayAsFloatX16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, FloatX16# #)
readFloatArrayAsFloatX4# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, FloatX4# #)
readFloatArrayAsFloatX8# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, FloatX8# #)
readFloatOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Float# #)
readFloatOffAddrAsFloatX16# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX16# #)
readFloatOffAddrAsFloatX4# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX4# #)
readFloatOffAddrAsFloatX8# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX8# #)
readFloatX16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, FloatX16# #)
readFloatX16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX16# #)
readFloatX4Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, FloatX4# #)
readFloatX4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX4# #)
readFloatX8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, FloatX8# #)
readFloatX8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, FloatX8# #)
readInt16Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt16ArrayAsInt16X16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int16X16# #)
readInt16ArrayAsInt16X32# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int16X32# #)
readInt16ArrayAsInt16X8# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int16X8# #)
readInt16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int# #)
readInt16OffAddrAsInt16X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X16# #)
readInt16OffAddrAsInt16X32# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X32# #)
readInt16OffAddrAsInt16X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X8# #)
readInt16X16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int16X16# #)
readInt16X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X16# #)
readInt16X32Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int16X32# #)
readInt16X32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X32# #)
readInt16X8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int16X8# #)
readInt16X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int16X8# #)
readInt32Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt32ArrayAsInt32X16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int32X16# #)
readInt32ArrayAsInt32X4# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int32X4# #)
readInt32ArrayAsInt32X8# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int32X8# #)
readInt32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int# #)
readInt32OffAddrAsInt32X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X16# #)
readInt32OffAddrAsInt32X4# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X4# #)
readInt32OffAddrAsInt32X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X8# #)
readInt32X16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Int32X16# #)
readInt32X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X16# #)
readInt32X4Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int32X4# #)
readInt32X4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X4# #)
readInt32X8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int32X8# #)
readInt32X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int32X8# #)
readInt64Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt64ArrayAsInt64X2# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X2# #)
readInt64ArrayAsInt64X4# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X4# #)
readInt64ArrayAsInt64X8# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X8# #)
readInt64OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int# #)
readInt64OffAddrAsInt64X2# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X2# #)
readInt64OffAddrAsInt64X4# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X4# #)
readInt64OffAddrAsInt64X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X8# #)
readInt64X2Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X2# #)
readInt64X2OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X2# #)
readInt64X4Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X4# #)
readInt64X4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X4# #)
readInt64X8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64X8# #)
readInt64X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int64X8# #)
readInt8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readInt8ArrayAsInt8X16# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X16# #)
readInt8ArrayAsInt8X32# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X32# #)
readInt8ArrayAsInt8X64# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X64# #)
readInt8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int# #)
readInt8OffAddrAsInt8X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X16# #)
readInt8OffAddrAsInt8X32# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X32# #)
readInt8OffAddrAsInt8X64# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X64# #)
readInt8X16Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X16# #)
readInt8X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X16# #)
readInt8X32Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X32# #)
readInt8X32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X32# #)
readInt8X64Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8X64# #)
readInt8X64OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int8X64# #)
readIntArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readIntOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Int# #)
readMVar# :: MVar# d a -> State# d -> (# State# d, a #)
readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)
readMutableArrayArrayArray# ::
  MutableArrayArray# d
  -> Int# -> State# d -> (# State# d, MutableArrayArray# d #)
readMutableByteArrayArray# ::
  MutableArrayArray# d
  -> Int# -> State# d -> (# State# d, MutableByteArray# d #)
readSmallArray# ::
  SmallMutableArray# d a -> Int# -> State# d -> (# State# d, a #)
readStablePtrArray# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, StablePtr# a #)
readStablePtrOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, StablePtr# a #)
readTVar# :: TVar# d a -> State# d -> (# State# d, a #)
readTVarIO# :: TVar# d a -> State# d -> (# State# d, a #)
readWideCharArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWideCharOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Char# #)
readWord16Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord16ArrayAsWord16X16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X16# #)
readWord16ArrayAsWord16X32# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X32# #)
readWord16ArrayAsWord16X8# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X8# #)
readWord16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord16OffAddrAsWord16X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X16# #)
readWord16OffAddrAsWord16X32# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X32# #)
readWord16OffAddrAsWord16X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X8# #)
readWord16X16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X16# #)
readWord16X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X16# #)
readWord16X32Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X32# #)
readWord16X32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X32# #)
readWord16X8Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word16X8# #)
readWord16X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word16X8# #)
readWord32Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord32ArrayAsWord32X16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X16# #)
readWord32ArrayAsWord32X4# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X4# #)
readWord32ArrayAsWord32X8# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X8# #)
readWord32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord32OffAddrAsWord32X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X16# #)
readWord32OffAddrAsWord32X4# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X4# #)
readWord32OffAddrAsWord32X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X8# #)
readWord32X16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X16# #)
readWord32X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X16# #)
readWord32X4Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X4# #)
readWord32X4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X4# #)
readWord32X8Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word32X8# #)
readWord32X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word32X8# #)
readWord64Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord64ArrayAsWord64X2# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X2# #)
readWord64ArrayAsWord64X4# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X4# #)
readWord64ArrayAsWord64X8# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X8# #)
readWord64OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord64OffAddrAsWord64X2# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X2# #)
readWord64OffAddrAsWord64X4# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X4# #)
readWord64OffAddrAsWord64X8# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X8# #)
readWord64X2Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X2# #)
readWord64X2OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X2# #)
readWord64X4Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X4# #)
readWord64X4OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X4# #)
readWord64X8Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word64X8# #)
readWord64X8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word64X8# #)
readWord8Array# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsAddr# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)
readWord8ArrayAsChar# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsDouble# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsFloat# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)
readWord8ArrayAsInt# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt16# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt64# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsStablePtr# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, StablePtr# a #)
readWord8ArrayAsWideChar# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsWord# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord16# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord32# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord64# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord8X16# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X16# #)
readWord8ArrayAsWord8X32# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X32# #)
readWord8ArrayAsWord8X64# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X64# #)
readWord8OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word# #)
readWord8OffAddrAsWord8X16# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X16# #)
readWord8OffAddrAsWord8X32# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X32# #)
readWord8OffAddrAsWord8X64# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X64# #)
readWord8X16Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X16# #)
readWord8X16OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X16# #)
readWord8X32Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X32# #)
readWord8X32OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X32# #)
readWord8X64Array# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, Word8X64# #)
readWord8X64OffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word8X64# #)
readWordArray# ::
  MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWordOffAddr# ::
  Addr# -> Int# -> State# d -> (# State# d, Word# #)
realWorld# :: State# RealWorld
reallyUnsafePtrEquality# :: a -> a -> Int#
remAddr# :: Addr# -> Int# -> Int#
remInt# :: Int# -> Int# -> Int#
remInt16# :: Int16# -> Int16# -> Int16#
remInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
remInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
remInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
remInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
remInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
remInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
remInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
remInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
remInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
remInt8# :: Int8# -> Int8# -> Int8#
remInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
remInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
remInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
remWord# :: Word# -> Word# -> Word#
remWord16# :: Word16# -> Word16# -> Word16#
remWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
remWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
remWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
remWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
remWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
remWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
remWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
remWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
remWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
remWord8# :: Word8# -> Word8# -> Word8#
remWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
remWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
remWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
resizeMutableByteArray# ::
  MutableByteArray# d
  -> Int# -> State# d -> (# State# d, MutableByteArray# d #)
retry# :: State# RealWorld -> (# State# RealWorld, a #)
sameMVar# :: MVar# d a -> MVar# d a -> Int#
sameMutVar# :: MutVar# d a -> MutVar# d a -> Int#
sameMutableArray# :: MutableArray# d a -> MutableArray# d a -> Int#
sameMutableArrayArray# ::
  MutableArrayArray# d -> MutableArrayArray# d -> Int#
sameMutableByteArray# ::
  MutableByteArray# d -> MutableByteArray# d -> Int#
sameSmallMutableArray# ::
  SmallMutableArray# d a -> SmallMutableArray# d a -> Int#
sameTVar# :: TVar# d a -> TVar# d a -> Int#
seq# :: a -> State# d -> (# State# d, a #)
setByteArray# ::
  MutableByteArray# d -> Int# -> Int# -> Int# -> State# d -> State# d
setThreadAllocationCounter# ::
  Int# -> State# RealWorld -> State# RealWorld
shrinkMutableByteArray# ::
  MutableByteArray# d -> Int# -> State# d -> State# d
sinDouble# :: Double# -> Double#
sinFloat# :: Float# -> Float#
sinhDouble# :: Double# -> Double#
sinhFloat# :: Float# -> Float#
sizeofArray# :: Array# a -> Int#
sizeofArrayArray# :: ArrayArray# -> Int#
sizeofByteArray# :: ByteArray# -> Int#
sizeofMutableArray# :: MutableArray# d a -> Int#
sizeofMutableArrayArray# :: MutableArrayArray# d -> Int#
sizeofMutableByteArray# :: MutableByteArray# d -> Int#
sizeofSmallArray# :: SmallArray# a -> Int#
sizeofSmallMutableArray# :: SmallMutableArray# d a -> Int#
spark# :: a -> State# d -> (# State# d, a #)
sqrtDouble# :: Double# -> Double#
sqrtFloat# :: Float# -> Float#
stableNameToInt# :: StableName# a -> Int#
subInt16# :: Int16# -> Int16# -> Int16#
subInt8# :: Int8# -> Int8# -> Int8#
subIntC# :: Int# -> Int# -> (# Int#, Int# #)
subWord16# :: Word16# -> Word16# -> Word16#
subWord8# :: Word8# -> Word8# -> Word8#
subWordC# :: Word# -> Word# -> (# Word#, Int# #)
tagToEnum# :: Int# -> a
takeMVar# :: MVar# d a -> State# d -> (# State# d, a #)
tanDouble# :: Double# -> Double#
tanFloat# :: Float# -> Float#
tanhDouble# :: Double# -> Double#
tanhFloat# :: Float# -> Float#
thawArray# ::
  Array# a
  -> Int# -> Int# -> State# d -> (# State# d, MutableArray# d a #)
thawSmallArray# ::
  SmallArray# a
  -> Int#
  -> Int#
  -> State# d
  -> (# State# d, SmallMutableArray# d a #)
threadStatus# ::
  ThreadId#
  -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
timesDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#
timesDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
timesDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#
timesFloat# :: Float# -> Float# -> Float#
timesFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#
timesFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
timesFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#
timesInt16# :: Int16# -> Int16# -> Int16#
timesInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
timesInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
timesInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
timesInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
timesInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
timesInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
timesInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
timesInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
timesInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
timesInt8# :: Int8# -> Int8# -> Int8#
timesInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
timesInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
timesInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
timesWord# :: Word# -> Word# -> Word#
timesWord16# :: Word16# -> Word16# -> Word16#
timesWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
timesWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
timesWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
timesWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
timesWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
timesWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
timesWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
timesWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
timesWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
timesWord8# :: Word8# -> Word8# -> Word8#
timesWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
timesWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
timesWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
touch# :: a -> State# RealWorld -> State# RealWorld
traceBinaryEvent# :: Addr# -> Int# -> State# d -> State# d
traceEvent# :: Addr# -> State# d -> State# d
traceMarker# :: Addr# -> State# d -> State# d
tryPutMVar# :: MVar# d a -> a -> State# d -> (# State# d, Int# #)
tryReadMVar# :: MVar# d a -> State# d -> (# State# d, Int#, a #)
tryTakeMVar# :: MVar# d a -> State# d -> (# State# d, Int#, a #)
uncheckedIShiftL# :: Int# -> Int# -> Int#
uncheckedIShiftRA# :: Int# -> Int# -> Int#
uncheckedIShiftRL# :: Int# -> Int# -> Int#
uncheckedShiftL# :: Word# -> Int# -> Word#
uncheckedShiftRL# :: Word# -> Int# -> Word#
unmaskAsyncExceptions# ::
  (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)
unpackClosure# :: a -> (# Addr#, ByteArray#, Array# b #)
unpackDoubleX2# :: DoubleX2# -> (# Double#, Double# #)
unpackDoubleX4# ::
  DoubleX4# -> (# Double#, Double#, Double#, Double# #)
unpackDoubleX8# ::
  DoubleX8#
  -> (# Double#, Double#, Double#, Double#, Double#, Double#,
        Double#, Double# #)
unpackFloatX16# ::
  FloatX16#
  -> (# Float#, Float#, Float#, Float#, Float#, Float#, Float#,
        Float#, Float#, Float#, Float#, Float#, Float#, Float#, Float#,
        Float# #)
unpackFloatX4# :: FloatX4# -> (# Float#, Float#, Float#, Float# #)
unpackFloatX8# ::
  FloatX8#
  -> (# Float#, Float#, Float#, Float#, Float#, Float#, Float#,
        Float# #)
unpackInt16X16# ::
  Int16X16#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt16X32# ::
  Int16X32#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt16X8# ::
  Int16X8# -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt32X16# ::
  Int32X16#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt32X4# :: Int32X4# -> (# Int#, Int#, Int#, Int# #)
unpackInt32X8# ::
  Int32X8# -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt64X2# :: Int64X2# -> (# Int#, Int# #)
unpackInt64X4# :: Int64X4# -> (# Int#, Int#, Int#, Int# #)
unpackInt64X8# ::
  Int64X8# -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt8X16# ::
  Int8X16#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt8X32# ::
  Int8X32#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackInt8X64# ::
  Int8X64#
  -> (# Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#,
        Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int#, Int# #)
unpackWord16X16# ::
  Word16X16#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord16X32# ::
  Word16X32#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord16X8# ::
  Word16X8#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord32X16# ::
  Word32X16#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord32X4# :: Word32X4# -> (# Word#, Word#, Word#, Word# #)
unpackWord32X8# ::
  Word32X8#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord64X2# :: Word64X2# -> (# Word#, Word# #)
unpackWord64X4# :: Word64X4# -> (# Word#, Word#, Word#, Word# #)
unpackWord64X8# ::
  Word64X8#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord8X16# ::
  Word8X16#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord8X32# ::
  Word8X32#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word# #)
unpackWord8X64# ::
  Word8X64#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word#,
        Word#, Word# #)
unsafeCoerce# :: a -> b
unsafeFreezeArray# ::
  MutableArray# d a -> State# d -> (# State# d, Array# a #)
unsafeFreezeArrayArray# ::
  MutableArrayArray# d -> State# d -> (# State# d, ArrayArray# #)
unsafeFreezeByteArray# ::
  MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)
unsafeFreezeSmallArray# ::
  SmallMutableArray# d a -> State# d -> (# State# d, SmallArray# a #)
unsafeThawArray# ::
  Array# a -> State# d -> (# State# d, MutableArray# d a #)
unsafeThawSmallArray# ::
  SmallArray# a -> State# d -> (# State# d, SmallMutableArray# d a #)
void# :: Void#
waitRead# :: Int# -> State# d -> State# d
waitWrite# :: Int# -> State# d -> State# d
word2Double# :: Word# -> Double#
word2Float# :: Word# -> Float#
word2Int# :: Word# -> Int#
writeAddrArray# ::
  MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d
writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# d -> State# d
writeArray# ::
  MutableArray# d a -> Int# -> a -> State# d -> State# d
writeArrayArrayArray# ::
  MutableArrayArray# d -> Int# -> ArrayArray# -> State# d -> State# d
writeByteArrayArray# ::
  MutableArrayArray# d -> Int# -> ByteArray# -> State# d -> State# d
writeCharArray# ::
  MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# d -> State# d
writeDoubleArray# ::
  MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeDoubleArrayAsDoubleX2# ::
  MutableByteArray# d -> Int# -> DoubleX2# -> State# d -> State# d
writeDoubleArrayAsDoubleX4# ::
  MutableByteArray# d -> Int# -> DoubleX4# -> State# d -> State# d
writeDoubleArrayAsDoubleX8# ::
  MutableByteArray# d -> Int# -> DoubleX8# -> State# d -> State# d
writeDoubleOffAddr# ::
  Addr# -> Int# -> Double# -> State# d -> State# d
writeDoubleOffAddrAsDoubleX2# ::
  Addr# -> Int# -> DoubleX2# -> State# d -> State# d
writeDoubleOffAddrAsDoubleX4# ::
  Addr# -> Int# -> DoubleX4# -> State# d -> State# d
writeDoubleOffAddrAsDoubleX8# ::
  Addr# -> Int# -> DoubleX8# -> State# d -> State# d
writeDoubleX2Array# ::
  MutableByteArray# d -> Int# -> DoubleX2# -> State# d -> State# d
writeDoubleX2OffAddr# ::
  Addr# -> Int# -> DoubleX2# -> State# d -> State# d
writeDoubleX4Array# ::
  MutableByteArray# d -> Int# -> DoubleX4# -> State# d -> State# d
writeDoubleX4OffAddr# ::
  Addr# -> Int# -> DoubleX4# -> State# d -> State# d
writeDoubleX8Array# ::
  MutableByteArray# d -> Int# -> DoubleX8# -> State# d -> State# d
writeDoubleX8OffAddr# ::
  Addr# -> Int# -> DoubleX8# -> State# d -> State# d
writeFloatArray# ::
  MutableByteArray# d -> Int# -> Float# -> State# d -> State# d
writeFloatArrayAsFloatX16# ::
  MutableByteArray# d -> Int# -> FloatX16# -> State# d -> State# d
writeFloatArrayAsFloatX4# ::
  MutableByteArray# d -> Int# -> FloatX4# -> State# d -> State# d
writeFloatArrayAsFloatX8# ::
  MutableByteArray# d -> Int# -> FloatX8# -> State# d -> State# d
writeFloatOffAddr# ::
  Addr# -> Int# -> Float# -> State# d -> State# d
writeFloatOffAddrAsFloatX16# ::
  Addr# -> Int# -> FloatX16# -> State# d -> State# d
writeFloatOffAddrAsFloatX4# ::
  Addr# -> Int# -> FloatX4# -> State# d -> State# d
writeFloatOffAddrAsFloatX8# ::
  Addr# -> Int# -> FloatX8# -> State# d -> State# d
writeFloatX16Array# ::
  MutableByteArray# d -> Int# -> FloatX16# -> State# d -> State# d
writeFloatX16OffAddr# ::
  Addr# -> Int# -> FloatX16# -> State# d -> State# d
writeFloatX4Array# ::
  MutableByteArray# d -> Int# -> FloatX4# -> State# d -> State# d
writeFloatX4OffAddr# ::
  Addr# -> Int# -> FloatX4# -> State# d -> State# d
writeFloatX8Array# ::
  MutableByteArray# d -> Int# -> FloatX8# -> State# d -> State# d
writeFloatX8OffAddr# ::
  Addr# -> Int# -> FloatX8# -> State# d -> State# d
writeInt16Array# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt16ArrayAsInt16X16# ::
  MutableByteArray# d -> Int# -> Int16X16# -> State# d -> State# d
writeInt16ArrayAsInt16X32# ::
  MutableByteArray# d -> Int# -> Int16X32# -> State# d -> State# d
writeInt16ArrayAsInt16X8# ::
  MutableByteArray# d -> Int# -> Int16X8# -> State# d -> State# d
writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeInt16OffAddrAsInt16X16# ::
  Addr# -> Int# -> Int16X16# -> State# d -> State# d
writeInt16OffAddrAsInt16X32# ::
  Addr# -> Int# -> Int16X32# -> State# d -> State# d
writeInt16OffAddrAsInt16X8# ::
  Addr# -> Int# -> Int16X8# -> State# d -> State# d
writeInt16X16Array# ::
  MutableByteArray# d -> Int# -> Int16X16# -> State# d -> State# d
writeInt16X16OffAddr# ::
  Addr# -> Int# -> Int16X16# -> State# d -> State# d
writeInt16X32Array# ::
  MutableByteArray# d -> Int# -> Int16X32# -> State# d -> State# d
writeInt16X32OffAddr# ::
  Addr# -> Int# -> Int16X32# -> State# d -> State# d
writeInt16X8Array# ::
  MutableByteArray# d -> Int# -> Int16X8# -> State# d -> State# d
writeInt16X8OffAddr# ::
  Addr# -> Int# -> Int16X8# -> State# d -> State# d
writeInt32Array# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt32ArrayAsInt32X16# ::
  MutableByteArray# d -> Int# -> Int32X16# -> State# d -> State# d
writeInt32ArrayAsInt32X4# ::
  MutableByteArray# d -> Int# -> Int32X4# -> State# d -> State# d
writeInt32ArrayAsInt32X8# ::
  MutableByteArray# d -> Int# -> Int32X8# -> State# d -> State# d
writeInt32OffAddr# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeInt32OffAddrAsInt32X16# ::
  Addr# -> Int# -> Int32X16# -> State# d -> State# d
writeInt32OffAddrAsInt32X4# ::
  Addr# -> Int# -> Int32X4# -> State# d -> State# d
writeInt32OffAddrAsInt32X8# ::
  Addr# -> Int# -> Int32X8# -> State# d -> State# d
writeInt32X16Array# ::
  MutableByteArray# d -> Int# -> Int32X16# -> State# d -> State# d
writeInt32X16OffAddr# ::
  Addr# -> Int# -> Int32X16# -> State# d -> State# d
writeInt32X4Array# ::
  MutableByteArray# d -> Int# -> Int32X4# -> State# d -> State# d
writeInt32X4OffAddr# ::
  Addr# -> Int# -> Int32X4# -> State# d -> State# d
writeInt32X8Array# ::
  MutableByteArray# d -> Int# -> Int32X8# -> State# d -> State# d
writeInt32X8OffAddr# ::
  Addr# -> Int# -> Int32X8# -> State# d -> State# d
writeInt64Array# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt64ArrayAsInt64X2# ::
  MutableByteArray# d -> Int# -> Int64X2# -> State# d -> State# d
writeInt64ArrayAsInt64X4# ::
  MutableByteArray# d -> Int# -> Int64X4# -> State# d -> State# d
writeInt64ArrayAsInt64X8# ::
  MutableByteArray# d -> Int# -> Int64X8# -> State# d -> State# d
writeInt64OffAddr# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeInt64OffAddrAsInt64X2# ::
  Addr# -> Int# -> Int64X2# -> State# d -> State# d
writeInt64OffAddrAsInt64X4# ::
  Addr# -> Int# -> Int64X4# -> State# d -> State# d
writeInt64OffAddrAsInt64X8# ::
  Addr# -> Int# -> Int64X8# -> State# d -> State# d
writeInt64X2Array# ::
  MutableByteArray# d -> Int# -> Int64X2# -> State# d -> State# d
writeInt64X2OffAddr# ::
  Addr# -> Int# -> Int64X2# -> State# d -> State# d
writeInt64X4Array# ::
  MutableByteArray# d -> Int# -> Int64X4# -> State# d -> State# d
writeInt64X4OffAddr# ::
  Addr# -> Int# -> Int64X4# -> State# d -> State# d
writeInt64X8Array# ::
  MutableByteArray# d -> Int# -> Int64X8# -> State# d -> State# d
writeInt64X8OffAddr# ::
  Addr# -> Int# -> Int64X8# -> State# d -> State# d
writeInt8Array# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeInt8ArrayAsInt8X16# ::
  MutableByteArray# d -> Int# -> Int8X16# -> State# d -> State# d
writeInt8ArrayAsInt8X32# ::
  MutableByteArray# d -> Int# -> Int8X32# -> State# d -> State# d
writeInt8ArrayAsInt8X64# ::
  MutableByteArray# d -> Int# -> Int8X64# -> State# d -> State# d
writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeInt8OffAddrAsInt8X16# ::
  Addr# -> Int# -> Int8X16# -> State# d -> State# d
writeInt8OffAddrAsInt8X32# ::
  Addr# -> Int# -> Int8X32# -> State# d -> State# d
writeInt8OffAddrAsInt8X64# ::
  Addr# -> Int# -> Int8X64# -> State# d -> State# d
writeInt8X16Array# ::
  MutableByteArray# d -> Int# -> Int8X16# -> State# d -> State# d
writeInt8X16OffAddr# ::
  Addr# -> Int# -> Int8X16# -> State# d -> State# d
writeInt8X32Array# ::
  MutableByteArray# d -> Int# -> Int8X32# -> State# d -> State# d
writeInt8X32OffAddr# ::
  Addr# -> Int# -> Int8X32# -> State# d -> State# d
writeInt8X64Array# ::
  MutableByteArray# d -> Int# -> Int8X64# -> State# d -> State# d
writeInt8X64OffAddr# ::
  Addr# -> Int# -> Int8X64# -> State# d -> State# d
writeIntArray# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# d -> State# d
writeMutVar# :: MutVar# d a -> a -> State# d -> State# d
writeMutableArrayArrayArray# ::
  MutableArrayArray# d
  -> Int# -> MutableArrayArray# d -> State# d -> State# d
writeMutableByteArrayArray# ::
  MutableArrayArray# d
  -> Int# -> MutableByteArray# d -> State# d -> State# d
writeSmallArray# ::
  SmallMutableArray# d a -> Int# -> a -> State# d -> State# d
writeStablePtrArray# ::
  MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d
writeStablePtrOffAddr# ::
  Addr# -> Int# -> StablePtr# a -> State# d -> State# d
writeTVar# :: TVar# d a -> a -> State# d -> State# d
writeWideCharArray# ::
  MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWideCharOffAddr# ::
  Addr# -> Int# -> Char# -> State# d -> State# d
writeWord16Array# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord16ArrayAsWord16X16# ::
  MutableByteArray# d -> Int# -> Word16X16# -> State# d -> State# d
writeWord16ArrayAsWord16X32# ::
  MutableByteArray# d -> Int# -> Word16X32# -> State# d -> State# d
writeWord16ArrayAsWord16X8# ::
  MutableByteArray# d -> Int# -> Word16X8# -> State# d -> State# d
writeWord16OffAddr# ::
  Addr# -> Int# -> Word# -> State# d -> State# d
writeWord16OffAddrAsWord16X16# ::
  Addr# -> Int# -> Word16X16# -> State# d -> State# d
writeWord16OffAddrAsWord16X32# ::
  Addr# -> Int# -> Word16X32# -> State# d -> State# d
writeWord16OffAddrAsWord16X8# ::
  Addr# -> Int# -> Word16X8# -> State# d -> State# d
writeWord16X16Array# ::
  MutableByteArray# d -> Int# -> Word16X16# -> State# d -> State# d
writeWord16X16OffAddr# ::
  Addr# -> Int# -> Word16X16# -> State# d -> State# d
writeWord16X32Array# ::
  MutableByteArray# d -> Int# -> Word16X32# -> State# d -> State# d
writeWord16X32OffAddr# ::
  Addr# -> Int# -> Word16X32# -> State# d -> State# d
writeWord16X8Array# ::
  MutableByteArray# d -> Int# -> Word16X8# -> State# d -> State# d
writeWord16X8OffAddr# ::
  Addr# -> Int# -> Word16X8# -> State# d -> State# d
writeWord32Array# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord32ArrayAsWord32X16# ::
  MutableByteArray# d -> Int# -> Word32X16# -> State# d -> State# d
writeWord32ArrayAsWord32X4# ::
  MutableByteArray# d -> Int# -> Word32X4# -> State# d -> State# d
writeWord32ArrayAsWord32X8# ::
  MutableByteArray# d -> Int# -> Word32X8# -> State# d -> State# d
writeWord32OffAddr# ::
  Addr# -> Int# -> Word# -> State# d -> State# d
writeWord32OffAddrAsWord32X16# ::
  Addr# -> Int# -> Word32X16# -> State# d -> State# d
writeWord32OffAddrAsWord32X4# ::
  Addr# -> Int# -> Word32X4# -> State# d -> State# d
writeWord32OffAddrAsWord32X8# ::
  Addr# -> Int# -> Word32X8# -> State# d -> State# d
writeWord32X16Array# ::
  MutableByteArray# d -> Int# -> Word32X16# -> State# d -> State# d
writeWord32X16OffAddr# ::
  Addr# -> Int# -> Word32X16# -> State# d -> State# d
writeWord32X4Array# ::
  MutableByteArray# d -> Int# -> Word32X4# -> State# d -> State# d
writeWord32X4OffAddr# ::
  Addr# -> Int# -> Word32X4# -> State# d -> State# d
writeWord32X8Array# ::
  MutableByteArray# d -> Int# -> Word32X8# -> State# d -> State# d
writeWord32X8OffAddr# ::
  Addr# -> Int# -> Word32X8# -> State# d -> State# d
writeWord64Array# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord64ArrayAsWord64X2# ::
  MutableByteArray# d -> Int# -> Word64X2# -> State# d -> State# d
writeWord64ArrayAsWord64X4# ::
  MutableByteArray# d -> Int# -> Word64X4# -> State# d -> State# d
writeWord64ArrayAsWord64X8# ::
  MutableByteArray# d -> Int# -> Word64X8# -> State# d -> State# d
writeWord64OffAddr# ::
  Addr# -> Int# -> Word# -> State# d -> State# d
writeWord64OffAddrAsWord64X2# ::
  Addr# -> Int# -> Word64X2# -> State# d -> State# d
writeWord64OffAddrAsWord64X4# ::
  Addr# -> Int# -> Word64X4# -> State# d -> State# d
writeWord64OffAddrAsWord64X8# ::
  Addr# -> Int# -> Word64X8# -> State# d -> State# d
writeWord64X2Array# ::
  MutableByteArray# d -> Int# -> Word64X2# -> State# d -> State# d
writeWord64X2OffAddr# ::
  Addr# -> Int# -> Word64X2# -> State# d -> State# d
writeWord64X4Array# ::
  MutableByteArray# d -> Int# -> Word64X4# -> State# d -> State# d
writeWord64X4OffAddr# ::
  Addr# -> Int# -> Word64X4# -> State# d -> State# d
writeWord64X8Array# ::
  MutableByteArray# d -> Int# -> Word64X8# -> State# d -> State# d
writeWord64X8OffAddr# ::
  Addr# -> Int# -> Word64X8# -> State# d -> State# d
writeWord8Array# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsAddr# ::
  MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d
writeWord8ArrayAsChar# ::
  MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsDouble# ::
  MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsFloat# ::
  MutableByteArray# d -> Int# -> Float# -> State# d -> State# d
writeWord8ArrayAsInt# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt16# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt64# ::
  MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsStablePtr# ::
  MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d
writeWord8ArrayAsWideChar# ::
  MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWord# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord16# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord32# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord64# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord8X16# ::
  MutableByteArray# d -> Int# -> Word8X16# -> State# d -> State# d
writeWord8ArrayAsWord8X32# ::
  MutableByteArray# d -> Int# -> Word8X32# -> State# d -> State# d
writeWord8ArrayAsWord8X64# ::
  MutableByteArray# d -> Int# -> Word8X64# -> State# d -> State# d
writeWord8OffAddr# ::
  Addr# -> Int# -> Word# -> State# d -> State# d
writeWord8OffAddrAsWord8X16# ::
  Addr# -> Int# -> Word8X16# -> State# d -> State# d
writeWord8OffAddrAsWord8X32# ::
  Addr# -> Int# -> Word8X32# -> State# d -> State# d
writeWord8OffAddrAsWord8X64# ::
  Addr# -> Int# -> Word8X64# -> State# d -> State# d
writeWord8X16Array# ::
  MutableByteArray# d -> Int# -> Word8X16# -> State# d -> State# d
writeWord8X16OffAddr# ::
  Addr# -> Int# -> Word8X16# -> State# d -> State# d
writeWord8X32Array# ::
  MutableByteArray# d -> Int# -> Word8X32# -> State# d -> State# d
writeWord8X32OffAddr# ::
  Addr# -> Int# -> Word8X32# -> State# d -> State# d
writeWord8X64Array# ::
  MutableByteArray# d -> Int# -> Word8X64# -> State# d -> State# d
writeWord8X64OffAddr# ::
  Addr# -> Int# -> Word8X64# -> State# d -> State# d
writeWordArray# ::
  MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# d -> State# d
xor# :: Word# -> Word# -> Word#
xorI# :: Int# -> Int# -> Int#
yield# :: State# RealWorld -> State# RealWorld
-- imported via Prelude, GHC.Prim
seq :: a -> b -> b
```
