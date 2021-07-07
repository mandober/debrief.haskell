# Prelude exports

* Prelude exports 256 items (mostly functions, some classes)
* Prelude mostly re-exports items defined elsewhere
- no item starts with (Prelude.) H, K, P, Q, U, X, Y, Z
- symbolically-named operators: 28
- math fn: 
- Data ctors: 9
- Type ctors: 17
  - `type`    : 6
  - `newtype` : 1
  - `data`    : 10
- Types: 17
  - numeric types: 6
  - textual types: 4
- Type Classes : 22
  - numeric classes: 7


Prelude.-
Prelude.!!
Prelude..
Prelude.*
Prelude.**
Prelude.*>
Prelude./
Prelude./=
Prelude.&&
Prelude.^
Prelude.^^
Prelude.+
Prelude.++
Prelude.<
Prelude.<*
Prelude.<*>
Prelude.<=
Prelude.<>
Prelude.<$
Prelude.<$>
Prelude.=<<
Prelude.==
Prelude.>
Prelude.>=
Prelude.>>
Prelude.>>=
Prelude.||
Prelude.$
Prelude.$!

Prelude.Bool        data Bool = False | True
  Prelude.False
  Prelude.True
Prelude.Maybe       data Maybe = Nothing | Just
  Prelude.Nothing
  Prelude.Just
Prelude.Either      data Either = Left | Right
  Prelude.Left
  Prelude.Right
Prelude.Ordering    data Ordering = LT | EQ | GT
  Prelude.LT
  Prelude.EQ
  Prelude.GT
Prelude.Char        data Char = GHC.Types.C# GHC.Prim.Char#
Prelude.String      type String = [Char]
Prelude.FilePath    type FilePath = String
Prelude.ShowS       type ShowS = String -> String
Prelude.ReadS       type ReadS a = String -> [(a, String)]
Prelude.IO          newtype IO a
Prelude.IOError     type IOError = GHC.IO.Exception.IOException

Prelude.Int         data Int
Prelude.Integer     data Integer
Prelude.Word        data Word
Prelude.Float       data Float = GHC.Types.F# GHC.Prim.Float#
Prelude.Double      data Double = GHC.Types.D# GHC.Prim.Double#
Prelude.Rational    type Rational = GHC.Real.Ratio Integer

Prelude.Num         class Num a
Prelude.Fractional  class Num a => Fractional a
Prelude.Integral    class (Real a, Enum a) => Integral a
Prelude.Floating    class Fractional a => Floating a
Prelude.Real        class (Num a, Ord a) => Real a
Prelude.RealFrac    class (Real a, Fractional a) => RealFrac a
Prelude.RealFloat   class (RealFrac a, Floating a) => RealFloat a

Prelude.Eq          class Eq a
Prelude.Ord         class Eq a => Ord a
Prelude.Enum        class Enum a
Prelude.Bounded     class Bounded a
Prelude.Show        class Show a
Prelude.Read        class Read a

Prelude.Semigroup   class Semigroup a
Prelude.Monoid      class Semigroup a => Monoid a
Prelude.Functor     class Functor a
Prelude.Applicative class Functor a => Applicative a
Prelude.Monad       class Applicative a => Monad a
Prelude.MonadFail   class Monad m => MonadFail (m :: * -> *)
Prelude.Foldable    class Foldable (t :: * -> *)
Prelude.Traversable class (Functor t, Foldable t) => Traversable (t :: * -> *)


Prelude.abs
Prelude.acos
Prelude.acosh
Prelude.all
Prelude.and
Prelude.any
Prelude.appendFile
Prelude.asin
Prelude.asinh
Prelude.asTypeOf
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
Prelude.foldl
Prelude.foldl1
Prelude.foldMap
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
Prelude.sequence_
Prelude.sequenceA
Prelude.show
Prelude.showChar
Prelude.showList
Prelude.showParen
Prelude.shows
Prelude.showsPrec
Prelude.showString
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
