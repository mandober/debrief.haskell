# Data.Char

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
