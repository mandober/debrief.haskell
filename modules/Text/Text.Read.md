# Text.Read

```hs
minPrec :: Prec
lex     :: ReadS String
get     :: ReadPrec Char
lexP    :: ReadPrec Lexeme
look    :: ReadPrec String

reads       :: forall a. Read a => ReadS a
read        :: forall a. Read a => String -> a
readMaybe   :: forall a. Read a => String -> Maybe a
readEither  :: forall a. Read a => String -> Either String a

readListDefault :: forall a. Read a => ReadS [a]
readParen       :: forall a. Bool -> ReadS a -> ReadS a
readPrec_to_S   :: forall a. ReadPrec a -> Int -> ReadS a

pfail               :: forall a. ReadPrec a
step                :: forall a. ReadPrec a -> ReadPrec a
reset               :: forall a. ReadPrec a -> ReadPrec a
parens              :: forall a. ReadPrec a -> ReadPrec a
(+++)               :: forall a. ReadPrec a -> ReadPrec a -> ReadPrec a
(<++)               :: forall a. ReadPrec a -> ReadPrec a -> ReadPrec a

readListPrecDefault :: forall a. Read a => ReadPrec [a]
choice              :: forall a. [ReadPrec a] -> ReadPrec a
prec                :: forall a. Prec -> ReadPrec a -> ReadPrec a
readS_to_Prec       :: forall a. (Int -> ReadS a) -> ReadPrec a

lift          :: Text.ParserCombinators.ReadP.ReadP a -> ReadPrec a
readP_to_Prec :: (Int -> Text.ParserCombinators.ReadP.ReadP a) -> ReadPrec a
readPrec_to_P :: ReadPrec a -> Int -> Text.ParserCombinators.ReadP.ReadP a




type Prec :: Type
type Prec = Int

type ReadS :: Type -> Type
type ReadS a = String -> [(a, String)]

type Read :: Type -> Constraint
class Read a where
  readsPrec     :: Int -> ReadS a
  readList      :: ReadS    [a]
  readPrec      :: ReadPrec  a
  readListPrec  :: ReadPrec [a]
  {-# MINIMAL readsPrec | readPrec #-}

type ReadPrec :: Type -> Type
newtype ReadPrec a = Text.ParserCombinators.ReadPrec.P
                     (Prec -> Text.ParserCombinators.ReadP.ReadP a)

type Lexeme :: Type
data Lexeme
  = Char    Char
  | String  String
  | Punc    String
  | Ident   String
  | Symbol  String
  | Number  Text.Read.Lex.Number
  | EOF
```
