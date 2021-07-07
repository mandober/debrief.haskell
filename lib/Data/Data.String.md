# Data.String

```hs
import GHC.String


type String = [Char]

-- only these are string-specific
-- the rest is in Data.List

lines   :: String -> [String]
unlines :: [String] -> String

unwords :: [String] -> String
words   :: String -> [String]


class IsString a where
    fromString :: String -> a
{-# MINIMAL fromString #-}
```
