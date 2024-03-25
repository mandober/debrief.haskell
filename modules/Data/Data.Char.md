# Data.Char


```hs
import Data.Char qualified as C

C.Char                  C.OtherLetter           C.isDigit
C.ClosePunctuation      C.OtherNumber           C.isHexDigit
C.ConnectorPunctuation  C.OtherPunctuation      C.isLatin1
C.Control               C.OtherSymbol           C.isLetter
C.CurrencySymbol        C.ParagraphSeparator    C.isLower
C.DashPunctuation       C.PrivateUse            C.isMark
C.DecimalNumber         C.Space                 C.isNumber
C.EnclosingMark         C.SpacingCombiningMark  C.isOctDigit
C.FinalQuote            C.Surrogate             C.isPrint
C.Format                C.TitlecaseLetter       C.isPunctuation
C.GeneralCategory       C.UppercaseLetter       C.isSeparator
C.InitialQuote          C.chr                   C.isSpace
C.LetterNumber          C.digitToInt            C.isSymbol
C.LineSeparator         C.generalCategory       C.isUpper
C.LowercaseLetter       C.intToDigit            C.lexLitChar
C.MathSymbol            C.isAlpha               C.ord
C.ModifierLetter        C.isAlphaNum            C.readLitChar
C.ModifierSymbol        C.isAscii               C.showLitChar
C.NonSpacingMark        C.isAsciiLower          C.toLower
C.NotAssigned           C.isAsciiUpper          C.toTitle
C.OpenPunctuation       C.isControl             C.toUpper
```

```hs
-- predicates
isAlpha         :: Char  -> Bool
isAlphaNum      :: Char  -> Bool
isAscii         :: Char  -> Bool
isAsciiLower    :: Char  -> Bool
isAsciiUpper    :: Char  -> Bool
isLatin1        :: Char  -> Bool
isLetter        :: Char  -> Bool
isUpper         :: Char  -> Bool
isLower         :: Char  -> Bool

isDigit         :: Char  -> Bool
isOctDigit      :: Char  -> Bool
isHexDigit      :: Char  -> Bool
isNumber        :: Char  -> Bool

isMark          :: Char  -> Bool
isPrint         :: Char  -> Bool
isSymbol        :: Char  -> Bool
isSeparator     :: Char  -> Bool
isPunctuation   :: Char  -> Bool
isSpace         :: Char  -> Bool
isControl       :: Char  -> Bool

-- case conversion
toLower         :: Char  -> Char
toUpper         :: Char  -> Char
toTitle         :: Char  -> Char

-- ordinal <=> character
ord             :: Char  -> Int
digitToInt      :: Char  -> Int
chr             :: Int   -> Char
intToDigit      :: Int   -> Char

-- rest
lexLitChar      :: ReadS String
readLitChar     :: ReadS Char
showLitChar     :: Char  -> ShowS
generalCategory :: Char  -> GeneralCategory

type Char :: Type
data Char = GHC.Types.C# GHC.Prim.Char#

type GeneralCategory :: Type
data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
```
