# Functions

* Types
  - Word
  - Char String
  - Double 
  - Float
    - floatDigits
    - decodeFloat
    - encodeFloat
    - floatRadix
    - floatRange
  - Fractional
  - Rational
  - Real RealFloat RealFrac
  - Int Integer
  - Bool (False, True)
    - and or not otherwise
    - `&& ||`
  - Ordering (EQ, GT, LT)
  - Maybe (Just, Nothing)
    - maybe
  - Either (Left,Right)
    - either
  - IO
  - IOError
  - FilePath
  - list: [a]
    - `!!`
    - `++`
    - elem notElem null
    - drop dropWhile
    - concat concatMap
    - map 
    - foldl foldl1 foldr foldr1 foldMap
    - cycle
    - filter
    - zip zip3
    - zipWith zipWith3
    - break :: (a -> Bool) -> [a] -> ([a], [a])
  * function: `-> r`
    - `$`
    - `$!`
    - `.`
    - id
    - flip 
    - const
  * tuple: `(a, b)`
    - curry uncurry
    - fst snd
    - unzip unzip3
  * unit: `()` 
* Typeclasses
  - Eq
    - `==  /=`
  - Ord
    - compare `<  <=  >  >=`
  - Enum
    - enumFrom enumFromThen enumFromThenTo enumFromTo
  - Bounded
  - Num
  - Integral
  - Floating
  - Semigroup
  - Monoid
    - mconcat mempty
    - `<>`  (mappend)
  - Applicative
    - `*>  <*  <*>`
  - Functor
    - `<$ <$> <$>`
    - fmap
  - Monad
    - `=<<  >>=  >>` fail
  - Foldable
  - Traversable
  - Read
  - ReadS
  - Show
  - ShowS
* Math
  - `+ - *  **  ^  ^^ /`
  - div divMod
  - floor ceiling
  - abs acos acosh asin asinh atan atan2 atanh
  - cos cosh tan tanh
  - exp exponent
  - even odd
  - gcd lcm
  - log logBase
  mod negate pi
  quot quotRem
  sqrt
  subtract

properFraction
scaleFloat
recip sin sinh rem
round



undefined :: a

- all :: Foldable t => (a -> Bool) -> t a -> Bool
- any :: Foldable t => (a -> Bool) -> t a -> Bool
- asTypeOf :: a -> a -> a
- error :: [Char] -> a
- errorWithoutStackTrace :: [Char] -> a

- fromEnum :: Enum a => a -> Int
fromInteger :: Num a => Integer -> a
fromIntegral :: (Integral a, Num b) => a -> b
fromRational :: Fractional a => Rational -> a
isDenormalized
isIEEE
isInfinite
isNaN
isNegativeZero
toInteger
toRational
truncate
realToFrac

significand
signum

getChar     :: IO Char
getLine     :: IO String
getContents :: IO String

ioError :: IOError -> IO a
userError :: String -> IOError

interact :: (String -> String) -> IO ()
appendFile :: FilePath -> String -> IO ()
writeFile  :: FilePath -> String -> IO ()
readFile   :: FilePath -> IO String

print :: Show a => a -> IO ()

putChar  :: Char   -> IO ()
putStr   :: String -> IO ()
putStrLn :: String -> IO ()

lex :: ReadS String

read :: Read a => String -> a

readIO
readList
readLn
readParen
reads
readsPrec

showChar   :: Char -> ShowS
showString :: String -> ShowS

show
showList
showParen
shows
showsPrec

lookup :: Eq a => a -> [(a, b)] -> Maybe b
until :: (a -> Bool) -> (a -> a) -> a -> a

pure
return
mappend

head
tail
init
last
length
map
mapM
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
iterate
span
repeat
replicate
reverse
traverse
scanl
scanl1
scanr
scanr1
splitAt
take
takeWhile

lines :: String -> [String]
unlines :: [String] -> String

words :: String -> [String]
unwords :: [String] -> String

min
max
minBound
maxBound
minimum
maximum

product
sum

pred
succ
toEnum

seq
sequence
sequenceA
sequence_







<!-- #region Prelude QUALIFIED -->
Prelude.Rational
Prelude.Real
Prelude.RealFloat
Prelude.RealFrac
Prelude.Semigroup
Prelude.Traversable
Prelude.Read
Prelude.ReadS
Prelude.Show
Prelude.ShowS
Prelude.String
Prelude.Word
Prelude.^
Prelude.^^
Prelude.abs
Prelude.acos
Prelude.acosh
Prelude.all
Prelude.and
Prelude.any
Prelude.appendFile
Prelude.asTypeOf
Prelude.asin
Prelude.asinh
Prelude.atan
Prelude.atan2
Prelude.atanh
Prelude.break
Prelude.ceiling
Prelude.compare
Prelude.concat
Prelude.concatMap
Prelude.const
Prelude.cos
Prelude.cosh
Prelude.curry
Prelude.cycle
Prelude.decodeFloat
Prelude.div
Prelude.divMod
Prelude.drop
Prelude.dropWhile
Prelude.either
Prelude.elem
Prelude.encodeFloat
Prelude.enumFrom
Prelude.enumFromThen
Prelude.enumFromThenTo
Prelude.enumFromTo
Prelude.error
Prelude.errorWithoutStackTrace
Prelude.even
Prelude.exp
Prelude.exponent
Prelude.fail
Prelude.filter
Prelude.flip
Prelude.floatDigits
Prelude.floatRadix
Prelude.floatRange
Prelude.floor
Prelude.fmap
Prelude.foldMap
Prelude.foldl
Prelude.foldl1
Prelude.foldr
Prelude.foldr1
Prelude.fromEnum
Prelude.fromInteger
Prelude.fromIntegral
Prelude.fromRational
Prelude.fst
Prelude.gcd
Prelude.getChar
Prelude.getContents
Prelude.getLine
Prelude.head
Prelude.id
Prelude.init
Prelude.interact
Prelude.ioError
Prelude.isDenormalized
Prelude.isIEEE
Prelude.isInfinite
Prelude.isNaN
Prelude.isNegativeZero
Prelude.iterate
Prelude.last
Prelude.lcm
Prelude.length
Prelude.lex
Prelude.lines
Prelude.log
Prelude.logBase
Prelude.lookup
Prelude.map
Prelude.mapM
Prelude.mapM_
Prelude.mappend
Prelude.max
Prelude.maxBound
Prelude.maximum
Prelude.maybe
Prelude.mconcat
Prelude.mempty
Prelude.min
Prelude.minBound
Prelude.minimum
Prelude.mod
Prelude.negate
Prelude.not
Prelude.notElem
Prelude.null
Prelude.odd
Prelude.or
Prelude.otherwise
Prelude.pi
Prelude.pred
Prelude.print
Prelude.product
Prelude.properFraction
Prelude.pure
Prelude.putChar
Prelude.putStr
Prelude.putStrLn
Prelude.quot
Prelude.quotRem
Prelude.read
Prelude.readFile
Prelude.readIO
Prelude.readList
Prelude.readLn
Prelude.readParen
Prelude.reads
Prelude.readsPrec
Prelude.realToFrac
Prelude.recip
Prelude.rem
Prelude.repeat
Prelude.replicate
Prelude.return
Prelude.reverse
Prelude.round
Prelude.scaleFloat
Prelude.scanl
Prelude.scanl1
Prelude.scanr
Prelude.scanr1
Prelude.seq
Prelude.sequence
Prelude.sequenceA
Prelude.sequence_
Prelude.show
Prelude.showChar
Prelude.showList
Prelude.showParen
Prelude.showString
Prelude.shows
Prelude.showsPrec
Prelude.significand
Prelude.signum
Prelude.sin
Prelude.sinh
Prelude.snd
Prelude.span
Prelude.splitAt
Prelude.sqrt
Prelude.subtract
Prelude.succ
Prelude.sum
Prelude.tail
Prelude.take
Prelude.takeWhile
Prelude.tan
Prelude.tanh
Prelude.toEnum
Prelude.toInteger
Prelude.toRational
Prelude.traverse
Prelude.truncate
Prelude.uncurry
Prelude.undefined
Prelude.unlines
Prelude.until
Prelude.unwords
Prelude.unzip
Prelude.unzip3
Prelude.userError
Prelude.words
Prelude.writeFile
Prelude.zip
Prelude.zip3
Prelude.zipWith
Prelude.zipWith3

<!-- #endregion -->

<!-- #region Modules -->

Control.Applicative
Control.Arrow
Control.Category
Control.Concurrent
Control.Concurrent.Chan
Control.Concurrent.MVar
Control.Concurrent.QSem
Control.Concurrent.QSemN
Control.Exception
Control.Exception.Base
Control.Monad
Control.Monad.Fail
Control.Monad.Fix
Control.Monad.IO.Class
Control.Monad.Instances
Control.Monad.ST
Control.Monad.ST.Lazy
Control.Monad.ST.Lazy.Safe
Control.Monad.ST.Lazy.Unsafe
Control.Monad.ST.Safe
Control.Monad.ST.Strict
Control.Monad.ST.Unsafe
Control.Monad.Zip

Data.Bifoldable
Data.Bifunctor
Data.Bitraversable
Data.Bits
Data.Bool
Data.Char
Data.Coerce
Data.Complex
Data.Containers.ListUtils
Data.Data
Data.Dynamic
Data.Either
Data.Eq
Data.Fixed
Data.Foldable
Data.Function
Data.Functor
Data.Functor.Classes
Data.Functor.Compose
Data.Functor.Const
Data.Functor.Contravariant
Data.Functor.Identity
Data.Functor.Product
Data.Functor.Sum
Data.Graph
Data.IORef
Data.Int
Data.IntMap
Data.IntMap.Internal
Data.IntMap.Internal.Debug
Data.IntMap.Lazy
Data.IntMap.Merge.Lazy
Data.IntMap.Merge.Strict
Data.IntMap.Strict
Data.IntSet
Data.IntSet.Internal
Data.Ix
Data.Kind
Data.List
Data.List.NonEmpty
Data.Map
Data.Map.Internal
Data.Map.Internal.Debug
Data.Map.Lazy
Data.Map.Merge.Lazy
Data.Map.Merge.Strict
Data.Map.Strict
Data.Map.Strict.Internal
Data.Maybe
Data.Monoid
Data.Ord
Data.Proxy
Data.Ratio
Data.STRef
Data.STRef.Lazy
Data.STRef.Strict
Data.Semigroup
Data.Sequence
Data.Sequence.Internal
Data.Sequence.Internal.Sorting
Data.Set
Data.Set.Internal
Data.String
Data.Text
Data.Text.Array
Data.Text.Encoding
Data.Text.Encoding.Error
Data.Text.Foreign
Data.Text.IO
Data.Text.Internal
Data.Text.Internal.Builder
Data.Text.Internal.Builder.Functions
Data.Text.Internal.Builder.Int.Digits
Data.Text.Internal.Builder.RealFloat.Functions
Data.Text.Internal.Encoding.Fusion
Data.Text.Internal.Encoding.Fusion.Common
Data.Text.Internal.Encoding.Utf16
Data.Text.Internal.Encoding.Utf32
Data.Text.Internal.Encoding.Utf8
Data.Text.Internal.Functions
Data.Text.Internal.Fusion
Data.Text.Internal.Fusion.CaseMapping
Data.Text.Internal.Fusion.Common
Data.Text.Internal.Fusion.Size
Data.Text.Internal.Fusion.Types
Data.Text.Internal.IO
Data.Text.Internal.Lazy
Data.Text.Internal.Lazy.Encoding.Fusion
Data.Text.Internal.Lazy.Fusion
Data.Text.Internal.Lazy.Search
Data.Text.Internal.Private
Data.Text.Internal.Read
Data.Text.Internal.Search
Data.Text.Internal.Unsafe
Data.Text.Internal.Unsafe.Char
Data.Text.Internal.Unsafe.Shift
Data.Text.Lazy
Data.Text.Lazy.Builder
Data.Text.Lazy.Builder.Int
Data.Text.Lazy.Builder.RealFloat
Data.Text.Lazy.Encoding
Data.Text.Lazy.IO
Data.Text.Lazy.Internal
Data.Text.Lazy.Read
Data.Text.Read
Data.Text.Unsafe
Data.Traversable
Data.Tree
Data.Tuple
Data.Type.Bool
Data.Type.Coercion
Data.Type.Equality
Data.Typeable
Data.Unique
Data.Version
Data.Void
Data.Word

Debug.Trace

Foreign
Foreign.C
Foreign.C.Error
Foreign.C.String
Foreign.C.Types
Foreign.Concurrent
Foreign.ForeignPtr
Foreign.ForeignPtr.Safe
Foreign.ForeignPtr.Unsafe
Foreign.Marshal
Foreign.Marshal.Alloc
Foreign.Marshal.Array
Foreign.Marshal.Error
Foreign.Marshal.Pool
Foreign.Marshal.Safe
Foreign.Marshal.Unsafe
Foreign.Marshal.Utils
Foreign.Ptr
Foreign.Safe
Foreign.StablePtr
Foreign.Storable

GHC.Arr
GHC.Base
GHC.ByteOrder
GHC.Char
GHC.Clock
GHC.Conc
GHC.Conc.IO
GHC.Conc.Signal
GHC.Conc.Sync
GHC.ConsoleHandler
GHC.Constants
GHC.Desugar
GHC.Enum
GHC.Environment
GHC.Err
GHC.Event
GHC.Exception
GHC.Exception.Type
GHC.ExecutionStack
GHC.ExecutionStack.Internal
GHC.Exts
GHC.Fingerprint
GHC.Fingerprint.Type
GHC.Float
GHC.Float.ConversionUtils
GHC.Float.RealFracMethods
GHC.Foreign
GHC.ForeignPtr
GHC.GHCi
GHC.Generics
GHC.IO
GHC.IO.Buffer
GHC.IO.BufferedIO
GHC.IO.Device
GHC.IO.Encoding
GHC.IO.Encoding.CodePage
GHC.IO.Encoding.Failure
GHC.IO.Encoding.Iconv
GHC.IO.Encoding.Latin1
GHC.IO.Encoding.Types
GHC.IO.Encoding.UTF16
GHC.IO.Encoding.UTF32
GHC.IO.Encoding.UTF8
GHC.IO.Exception
GHC.IO.FD
GHC.IO.Handle
GHC.IO.Handle.FD
GHC.IO.Handle.Internals
GHC.IO.Handle.Lock
GHC.IO.Handle.Text
GHC.IO.Handle.Types
GHC.IO.IOMode
GHC.IO.Unsafe
GHC.IOArray
GHC.IORef
GHC.Int
GHC.List
GHC.MVar
GHC.Maybe
GHC.Natural
GHC.Num
GHC.OldList
GHC.OverloadedLabels
GHC.Pack
GHC.Profiling
GHC.Ptr
GHC.RTS.Flags
GHC.Read
GHC.Real
GHC.Records
GHC.ResponseFile
GHC.ST
GHC.STRef
GHC.Show
GHC.Stable
GHC.StableName
GHC.Stack
GHC.Stack.CCS
GHC.Stack.Types
GHC.StaticPtr
GHC.Stats
GHC.Storable
GHC.TopHandler
GHC.TypeLits
GHC.TypeNats
GHC.Unicode
GHC.Weak
GHC.Word

Numeric
Numeric.Natural

System.CPUTime
System.Console.GetOpt
System.Environment
System.Environment.Blank
System.Exit
System.IO
System.IO.Error
System.IO.Unsafe
System.Info
System.Mem
System.Mem.StableName
System.Mem.Weak
System.Posix.Internals
System.Posix.Types
System.Timeout

Text.ParserCombinators.ReadP
Text.ParserCombinators.ReadPrec
Text.Printf
Text.Read
Text.Read.Lex
Text.Show
Text.Show.Functions

Type.Reflection
Type.Reflection.Unsafe

Unsafe.Coerce

Utils.Containers.Internal.BitQueue
Utils.Containers.Internal.BitUtil
Utils.Containers.Internal.StrictPair

<!-- #endregion -->
