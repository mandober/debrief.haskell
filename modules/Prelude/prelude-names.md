# Names in Prelude

Additionally importing the `Prelude` and qualifying the import as `P`, makes it easier to list the 256 names (re)exported by the Prelude.

```hs
import Prelude qualified as P
-- in ghci, type: P.<TAB>
Display all 256 possibilities? (y or n)
```

## Prelude names

```
P.!!                      P.atan2                   P.notElem
P.$                       P.atanh                   P.null
P.$!                      P.break                   P.odd
P.&&                      P.ceiling                 P.or
P.*                       P.compare                 P.otherwise
P.**                      P.concat                  P.pi
P.*>                      P.concatMap               P.pred
P.+                       P.const                   P.print
P.++                      P.cos                     P.product
P.-                       P.cosh                    P.properFraction
P..                       P.curry                   P.pure
P./                       P.cycle                   P.putChar
P./=                      P.decodeFloat             P.putStr
P.<                       P.div                     P.putStrLn
P.<$                      P.divMod                  P.quot
P.<$>                     P.drop                    P.quotRem
P.<*                      P.dropWhile               P.read
P.<*>                     P.either                  P.readFile
P.<=                      P.elem                    P.readIO
P.<>                      P.encodeFloat             P.readList
P.=<<                     P.enumFrom                P.readLn
P.==                      P.enumFromThen            P.readParen
P.>                       P.enumFromThenTo          P.reads
P.>=                      P.enumFromTo              P.readsPrec
P.>>                      P.error                   P.realToFrac
P.>>=                     P.errorWithoutStackTrace  P.recip
P.Applicative             P.even                    P.rem
P.Bool                    P.exp                     P.repeat
P.Bounded                 P.exponent                P.replicate
P.Char                    P.fail                    P.return
P.Double                  P.filter                  P.reverse
P.EQ                      P.flip                    P.round
P.Either                  P.floatDigits             P.scaleFloat
P.Enum                    P.floatRadix              P.scanl
P.Eq                      P.floatRange              P.scanl1
P.False                   P.floor                   P.scanr
P.FilePath                P.fmap                    P.scanr1
P.Float                   P.foldMap                 P.seq
P.Floating                P.foldl                   P.sequence
P.Foldable                P.foldl1                  P.sequenceA
P.Fractional              P.foldr                   P.sequence_
P.Functor                 P.foldr1                  P.show
P.GT                      P.fromEnum                P.showChar
P.IO                      P.fromInteger             P.showList
P.IOError                 P.fromIntegral            P.showParen
P.Int                     P.fromRational            P.showString
P.Integer                 P.fst                     P.shows
P.Integral                P.gcd                     P.showsPrec
P.Just                    P.getChar                 P.significand
P.LT                      P.getContents             P.signum
P.Left                    P.getLine                 P.sin
P.Maybe                   P.head                    P.sinh
P.Monad                   P.id                      P.snd
P.MonadFail               P.init                    P.span
P.Monoid                  P.interact                P.splitAt
P.Nothing                 P.ioError                 P.sqrt
P.Num                     P.isDenormalized          P.subtract
P.Ord                     P.isIEEE                  P.succ
P.Ordering                P.isInfinite              P.sum
P.Rational                P.isNaN                   P.tail
P.Read                    P.isNegativeZero          P.take
P.ReadS                   P.iterate                 P.takeWhile
P.Real                    P.last                    P.tan
P.RealFloat               P.lcm                     P.tanh
P.RealFrac                P.length                  P.toEnum
P.Right                   P.lex                     P.toInteger
P.Semigroup               P.lines                   P.toRational
P.Show                    P.log                     P.traverse
P.ShowS                   P.logBase                 P.truncate
P.String                  P.lookup                  P.uncurry
P.Traversable             P.map                     P.undefined
P.True                    P.mapM                    P.unlines
P.Word                    P.mapM_                   P.until
P.^                       P.mappend                 P.unwords
P.^^                      P.max                     P.unzip
P.abs                     P.maxBound                P.unzip3
P.acos                    P.maximum                 P.userError
P.acosh                   P.maybe                   P.words
P.all                     P.mconcat                 P.writeFile
P.and                     P.mempty                  P.zip
P.any                     P.min                     P.zip3
P.appendFile              P.minBound                P.zipWith
P.asTypeOf                P.minimum                 P.zipWith3
P.asin                    P.mod                     P.||
P.asinh                   P.negate
P.atan                    P.not
```
