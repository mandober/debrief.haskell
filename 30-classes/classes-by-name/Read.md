
**Read**
- is sort of the opposite typeclass of Show
- `read` function takes a string and returns a type which is a member of Read
- All types covered so far except for functions are a part of Read


```hs
class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]
    GHC.Read.readPrec     :: Text.ParserCombinators.ReadPrec.ReadPrec a
    GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec [a]
-- MINIMAL: readsPrec | readPrec
```
