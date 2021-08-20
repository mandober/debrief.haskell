# Functions by purpose


## Characters

Prelude
Data.Char
GHC.Base
GHC.Char
GHC.Unicode

```hs
import Data.Char

GHC.Base.ord         :: Char -> Int -- Char to Int (in Data.Char)
GHC.Char.chr         :: Int -> Char -- Int to Char (in Data.Char)
GHC.Show.intToDigit  :: Int -> Char -- intToDigit 5 --> '5'
Data.Char.digitToInt :: Char -> Int

GHC.Unicode.toLower :: Char -> Char
GHC.Unicode.toTitle :: Char -> Char
GHC.Unicode.toUpper :: Char -> Char

-- PREDICATES:
GHC.Unicode.{PREDICATE} :: Char -> Bool
-- isAlpha, isAlphaNum, isAscii, isAsciiLower, isAsciiUpper
-- isDigit, isOctDigit, isHexDigit, isLower, isUpper
-- isSymbol, isControl, isPrint, isPunctuation, isSpace, isLatin1
Data.Char.isLetter    :: Char -> Bool
Data.Char.isMark      :: Char -> Bool
Data.Char.isNumber    :: Char -> Bool
Data.Char.isSeparator :: Char -> Bool

GHC.Classes.eqChar :: Char -> Char -> Bool
GHC.Classes.neChar :: Char -> Char -> Bool


```


## Strings



## Numbers
