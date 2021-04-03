# Text.Printf

```hs
import Text.Printf

-- imported via Text.Printf
SignPlus  :: FormatSign
SignSpace :: FormatSign
ZeroPad   :: FormatAdjustment

FieldFormat :: Maybe Int
            -> Maybe Int
            -> Maybe FormatAdjustment
            -> Maybe FormatSign
            -> Bool
            -> String
            -> Char
            -> FieldFormat

data FieldFormat      = ...E
data FormatAdjustment = ...E
data FormatParse      = ...E
data FormatSign       = ...E

FormatParse :: String -> Char -> String -> FormatParse
LeftAdjust :: FormatAdjustment

type FieldFormatter = FieldFormat -> ShowS
type ModifierParser = String -> FormatParse

errorBadArgument        :: a
errorShortFormat        :: a
errorMissingArgument    :: a
errorBadFormat  :: Char -> a
fmtAdjust       :: FieldFormat -> Maybe FormatAdjustment
fmtAlternate    :: FieldFormat -> Bool
fmtChar         :: FieldFormat -> Char
fmtModifiers    :: FieldFormat -> String
fmtPrecision    :: FieldFormat -> Maybe Int
fmtSign         :: FieldFormat -> Maybe FormatSign
fmtWidth        :: FieldFormat -> Maybe Int

formatArg       :: PrintfArg a => a -> FieldFormatter
formatChar      :: Char -> FieldFormatter
formatInt       :: (Integral a, Bounded a) => a -> FieldFormatter
formatInteger   :: Integer -> FieldFormatter
formatRealFloat :: RealFloat a => a -> FieldFormatter
formatString    :: IsChar a => [a] -> FieldFormatter

fpChar          :: FormatParse -> Char
fpModifiers     :: FormatParse -> String
fpRest          :: FormatParse -> String

perror          :: String -> a
vFmt            :: Char -> FieldFormat -> FieldFormat
fromChar        :: IsChar c => Char -> c
toChar          :: IsChar c => c -> Char
parseFormat     :: PrintfArg a => a -> ModifierParser
printf          :: PrintfType r => String -> r
hPrintf         :: HPrintfType r => GHC.IO.Handle.Types.Handle -> String -> r

class IsChar      c
class PrintfArg   a
class PrintfType  t
class HPrintfType t
```
