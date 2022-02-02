# Text.Parsec.Token

```hs
type Text.Parsec.Token.GenLanguageDef :: Type
                                         -> Type -> (Type -> Type) -> Type
data Text.Parsec.Token.GenLanguageDef s u m
  = Text.Parsec.Token.LanguageDef 
    {Text.Parsec.Token.commentStart :: String,
    Text.Parsec.Token.commentEnd :: String,
    Text.Parsec.Token.commentLine :: String,
    Text.Parsec.Token.nestedComments :: Bool,
    Text.Parsec.Token.identStart :: ParsecT s u m Char,
    Text.Parsec.Token.identLetter :: ParsecT s u m Char,
    Text.Parsec.Token.opStart :: ParsecT s u m Char,
    Text.Parsec.Token.opLetter :: ParsecT s u m Char,
    Text.Parsec.Token.reservedNames :: [String],
    Text.Parsec.Token.reservedOpNames :: [String],
    Text.Parsec.Token.caseSensitive :: Bool}

type Text.Parsec.Token.GenTokenParser :: Type
  -> Type -> (Type -> Type) -> Type

data Text.Parsec.Token.GenTokenParser s u m = Text.Parsec.Token.TokenParser -- {
 Text.Parsec.Token.reserved       :: String -> ParsecT s u m (),
 Text.Parsec.Token.reservedOp     :: String -> ParsecT s u m (),
 Text.Parsec.Token.symbol         :: String -> ParsecT s u m String,
 Text.Parsec.Token.identifier     :: ParsecT s u m String,
 Text.Parsec.Token.operator       :: ParsecT s u m String,
 Text.Parsec.Token.stringLiteral  :: ParsecT s u m String,
 Text.Parsec.Token.semi           :: ParsecT s u m String,
 Text.Parsec.Token.comma          :: ParsecT s u m String,
 Text.Parsec.Token.colon          :: ParsecT s u m String,
 Text.Parsec.Token.dot            :: ParsecT s u m String,
 Text.Parsec.Token.whiteSpace     :: ParsecT s u m (),
 Text.Parsec.Token.natural        :: ParsecT s u m Integer,
 Text.Parsec.Token.integer        :: ParsecT s u m Integer,
 Text.Parsec.Token.decimal        :: ParsecT s u m Integer,
 Text.Parsec.Token.hexadecimal    :: ParsecT s u m Integer,
 Text.Parsec.Token.octal          :: ParsecT s u m Integer,
 Text.Parsec.Token.naturalOrFloat :: ParsecT s u m (Either Integer Double),
 Text.Parsec.Token.charLiteral    :: ParsecT s u m Char,
 Text.Parsec.Token.float          :: ParsecT s u m Double,

 Text.Parsec.Token.lexeme    :: forall a. ParsecT s u m a -> ParsecT s u m a,
 Text.Parsec.Token.parens    :: forall a. ParsecT s u m a -> ParsecT s u m a,
 Text.Parsec.Token.braces    :: forall a. ParsecT s u m a -> ParsecT s u m a,
 Text.Parsec.Token.angles    :: forall a. ParsecT s u m a -> ParsecT s u m a,
 Text.Parsec.Token.brackets  :: forall a. ParsecT s u m a -> ParsecT s u m a,
 Text.Parsec.Token.squares   :: forall a. ParsecT s u m a -> ParsecT s u m a,

 Text.Parsec.Token.semiSep   :: forall a. ParsecT s u m a -> ParsecT s u m [a],
 Text.Parsec.Token.semiSep1  :: forall a. ParsecT s u m a -> ParsecT s u m [a],
 Text.Parsec.Token.commaSep  :: forall a. ParsecT s u m a -> ParsecT s u m [a],
 Text.Parsec.Token.commaSep1 :: forall a. ParsecT s u m a -> ParsecT s u m [a]
 }

type Text.Parsec.Token.LanguageDef :: Type -> Type
type Text.Parsec.Token.LanguageDef st =
  Text.Parsec.Token.GenLanguageDef
    String st Data.Functor.Identity.Identity

type Text.Parsec.Token.TokenParser :: Type -> Type
type Text.Parsec.Token.TokenParser st =
  Text.Parsec.Token.GenTokenParser
    String st Data.Functor.Identity.Identity

Text.Parsec.Token.makeTokenParser ::
  forall s (m :: Type -> Type) u.
  Stream s m Char
  => Text.Parsec.Token.GenLanguageDef s u m
  -> Text.Parsec.Token.GenTokenParser s u m
```
