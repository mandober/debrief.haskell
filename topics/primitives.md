# Primitives

* Docs: https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Exts.html
* module: `GHC.Exts`


* Basic types

- `Int#`    ctor `I# Int#`      data Int    = I# Int#
- `Word#`   ctor `W# Word#`     data Word   = I# Word#
- `Float#`  ctor `F# Float#`    data Float  = I# Float#
- `Double#` ctor `D# Double#`   data Double = I# Double#
- `Char#`   ctor `C# Char#`     data Char   = I# Char#
- data Ptr a    = Ptr Addr#
- data FunPtr a = FunPtr Addr#


* Primitive operations

unsafeCoerce# :: forall (k0 :: RuntimeRep) (k1 :: RuntimeRep) 
                        (a :: TYPE k0)     (b :: TYPE k1)    . a -> b

void#           :: Void#
realWorld#      :: State# RealWorld
nullAddr#       :: Addr#
magicDict       :: a
proxy#          :: forall k (a :: k). Proxy# a
data Addr#      :: TYPE 'AddrRep
data Array# a   :: TYPE 'UnliftedRep
data ByteArray# :: TYPE 'UnliftedRep

data Char#   :: TYPE 'WordRep
data Double# :: TYPE 'DoubleRep
data Float#  :: TYPE 'FloatRep
data Int#    :: TYPE 'IntRep
data Int8#   :: TYPE 'Int8Rep
data Int16#  :: TYPE 'Int16Rep
data Int32#  :: TYPE 'Int32Rep
data Int64#  :: TYPE 'Int64Rep

data RealWorld
data State# a :: TYPE ('TupleRep ('[] :: [RuntimeRep]))
data Void# :: TYPE ('TupleRep ('[] :: [RuntimeRep]))
data Word# :: TYPE 'WordRep
data Word8# :: TYPE 'Word8Rep
data Word16# :: TYPE 'Word16Rep
data Word32# :: TYPE 'Word32Rep
data Word64# :: TYPE 'Word64Rep
data ThreadId# :: TYPE 'UnliftedRep
