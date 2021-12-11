# Text.Parsec

```hs
Text.Parsec                  Text.Parsec.Error            Text.Parsec.Prim
Text.Parsec.ByteString       Text.Parsec.Expr             Text.Parsec.String
Text.Parsec.ByteString.Lazy  Text.Parsec.Language         Text.Parsec.Text
Text.Parsec.Char             Text.Parsec.Perm             Text.Parsec.Text.Lazy
Text.Parsec.Combinator       Text.Parsec.Pos              Text.Parsec.Token

>>> λ :bro Text.Parsec

(<?>) :: forall s u (m :: Type -> Type) a.
         ParsecT s u m a -> String -> ParsecT s u m a

(<|>) :: forall s u (m :: Type -> Type) a.
         ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a


satisfy :: forall s (m :: Type -> Type) u. Stream s m Char =>
           (Char -> Bool) -> ParsecT s u m Char

digit     :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
hexDigit  :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
octDigit  :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
alphaNum  :: forall s (m :: Type -> Type) u. Stream s m Char => 
             ParsecT s u m Char

char      :: forall s (m :: Type -> Type) u. Stream s m Char =>
             Char -> ParsecT s u m Char

letter    :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char

eof       :: forall s (m :: Type -> Type) t u. (Stream s m t, Show t) => 
             ParsecT s u m ()
crlf      :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
newline   :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
endOfLine :: forall s (m :: Type -> Type) u. Stream s m Char =>
             ParsecT s u m Char
tab    :: forall s (m :: Type -> Type) u. Stream s m Char => ParsecT s u m Char
space  :: forall s (m :: Type -> Type) u. Stream s m Char => ParsecT s u m Char
spaces :: forall s (m :: Type -> Type) u. Stream s m Char => ParsecT s u m ()
string :: forall s (m :: Type -> Type) u. Stream s m Char =>
          String -> ParsecT s u m String

lower :: forall s (m :: Type -> Type) u. Stream s m Char => ParsecT s u m Char
upper :: forall s (m :: Type -> Type) u. Stream s m Char => ParsecT s u m Char

anyChar :: forall s (m :: Type -> Type) u. Stream s m Char =>
           ParsecT s u m Char
anyToken :: forall s (m :: Type -> Type) t u. (Stream s m t, Show t) => 
            ParsecT s u m t

label  :: forall s u (m :: Type -> Type) a.
          ParsecT s u m a -> String -> ParsecT s u m a
labels :: forall s u (m :: Type -> Type) a.
          ParsecT s u m a -> [String] -> ParsecT s u m a

choice :: forall s (m :: Type -> Type) t u a. Stream s m t =>
          [ParsecT s u m a] -> ParsecT s u m a

count :: forall s (m :: Type -> Type) t u a. Stream s m t =>
         Int -> ParsecT s u m a -> ParsecT s u m [a]

endBy  :: forall s (m :: Type -> Type) t u a sep. Stream s m t =>
          ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 :: forall s (m :: Type -> Type) t u a sep. Stream s m t =>
          ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]

between :: forall s (m :: Type -> Type) t u open close a. Stream s m t =>
           ParsecT s u m open
        -> ParsecT s u m close
        -> ParsecT s u m a
        -> ParsecT s u m a

noneOf :: forall s (m :: Type -> Type) u. Stream s m Char =>
          [Char] -> ParsecT s u m Char

notFollowedBy ::
  forall s (m :: Type -> Type) t a u.
  (Stream s m t, Show a) =>
  ParsecT s u m a -> ParsecT s u m ()

oneOf ::
  forall s (m :: Type -> Type) u.
  Stream s m Char =>
  [Char] -> ParsecT s u m Char

option :: forall s (m :: Type -> Type) t a u. Stream s m t =>
          a -> ParsecT s u m a -> ParsecT s u m a
optionMaybe :: forall s (m :: Type -> Type) t u a. Stream s m t =>
               ParsecT s u m a -> ParsecT s u m (Maybe a)
optional :: forall s (m :: Type -> Type) t u a. Stream s m t =>
            ParsecT s u m a -> ParsecT s u m ()

many ::
  forall s u (m :: Type -> Type) a.
  ParsecT s u m a -> ParsecT s u m [a]
many1 ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m [a]
manyAccum ::
  forall a s u (m :: Type -> Type).
  (a -> [a] -> [a]) -> ParsecT s u m a -> ParsecT s u m [a]
manyTill ::
  forall s (m :: Type -> Type) t u a end.
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]

token :: forall s t a u. Stream s Data.Functor.Identity.Identity t =>
         (t -> String) -> (t -> SourcePos) -> (t -> Maybe a) -> Parsec s u a
tokens :: forall s (m :: Type -> Type) t u. (Stream s m t, Eq t) =>
  ([t] -> String) -> (SourcePos -> [t] -> SourcePos) -> [t] -> ParsecT s u m [t]
tokenPrim ::
  forall s (m :: Type -> Type) t a u.
  Stream s m t =>
  (t -> String)
  -> (SourcePos -> t -> s -> SourcePos)
  -> (t -> Maybe a)
  -> ParsecT s u m a
tokenPrimEx ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  (t -> String)
  -> (SourcePos -> t -> s -> SourcePos)
  -> Maybe (SourcePos -> t -> s -> u -> u)
  -> (t -> Maybe a)
  -> ParsecT s u m a

try :: forall s u (m :: Type -> Type) a. ParsecT s u m a -> ParsecT s u m a

sepBy :: forall s (m :: Type -> Type) t u a sep. Stream s m t =>
         ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 :: forall s (m :: Type -> Type) t u a sep. Stream s m t =>
         ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]

sepEndBy ::  forall s (m :: Type -> Type) t u a sep. Stream s m t =>
             ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 :: forall s (m :: Type -> Type) t u a sep. Stream s m t =>
             ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]

lookAhead :: forall s (m :: Type -> Type) t u a. Stream s m t =>
             ParsecT s u m a -> ParsecT s u m a

skipMany ::
  forall s u (m :: Type -> Type) a.
  ParsecT s u m a -> ParsecT s u m ()
skipMany1 ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m ()

chainl ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t => ParsecT s u m a
  -> ParsecT s u m (a -> a -> a)
  -> a
  -> ParsecT s u m a
chainl1 ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a
  -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr1 ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a


-- ----------------------------------------------------------------------------
getInput ::
  forall (m :: Type -> Type) s u. Monad m => ParsecT s u m s
getParserState ::
  forall (m :: Type -> Type) s u.
  Monad m =>
  ParsecT s u m (State s u)
getPosition ::
  forall (m :: Type -> Type) s u. Monad m => ParsecT s u m SourcePos
errorPos :: ParseError -> SourcePos

incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceLine :: SourcePos -> Line -> SourcePos

mergeErrorReply ::
  forall s u a. ParseError -> Reply s u a -> Reply s u a
mkPT ::
  forall (m :: Type -> Type) s u a.
  Monad m =>
  (State s u -> m (Consumed (m (Reply s u a)))) -> ParsecT s u m a

parse ::
  forall s t a.
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s () a -> SourceName -> s -> Either ParseError a
parseTest ::
  forall s t a.
  (Stream s Data.Functor.Identity.Identity t, Show a) =>
  Parsec s () a -> s -> IO ()
parsecMap ::
  forall a b s u (m :: Type -> Type).
  (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parserBind ::
  forall s u (m :: Type -> Type) a b.
  ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
parserFail ::
  forall s u (m :: Type -> Type) a. String -> ParsecT s u m a
parserPlus ::
  forall s u (m :: Type -> Type) a.
  ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
parserReturn ::
  forall a s u (m :: Type -> Type). a -> ParsecT s u m a
parserTrace ::
  forall t s (m :: Type -> Type) u.
  (Show t, Stream s m t) =>
  String -> ParsecT s u m ()
parserTraced ::
  forall s (m :: Type -> Type) t u b.
  (Stream s m t, Show t) =>
  String -> ParsecT s u m b -> ParsecT s u m b
parserZero :: forall s u (m :: Type -> Type) a. ParsecT s u m a

runP ::
  forall s t u a.
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runPT ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParsecT ::
  forall (m :: Type -> Type) s u a.
  Monad m =>
  ParsecT s u m a -> State s u -> m (Consumed (m (Reply s u a)))
runParser ::
  forall s t u a.
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParserT ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)

setInput ::
  forall (m :: Type -> Type) s u. Monad m => s -> ParsecT s u m ()
setPosition ::
  forall (m :: Type -> Type) s u.
  Monad m =>
  SourcePos -> ParsecT s u m ()
setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceLine :: SourcePos -> Line -> SourcePos
setSourceName :: SourcePos -> SourceName -> SourcePos

sourceColumn :: SourcePos -> Column
sourceLine :: SourcePos -> Line
sourceName :: SourcePos -> SourceName

sysUnExpectError ::
  forall s u a. String -> SourcePos -> Reply s u a
unexpected ::
  forall s (m :: Type -> Type) t u a.
  Stream s m t =>
  String -> ParsecT s u m a
unknownError :: forall s u. State s u -> ParseError

updateParserState :: forall s u (m :: Type -> Type).
                     (State s u -> State s u) -> ParsecT s u m (State s u)

setParserState :: forall (m :: Type -> Type) s u. Monad m =>
                  State s u -> ParsecT s u m (State s u)

putState :: forall (m :: Type -> Type) u s. Monad m => u -> ParsecT s u m ()
setState :: forall (m :: Type -> Type) u s. Monad m => u -> ParsecT s u m ()
getState :: forall (m :: Type -> Type) s u. Monad m => ParsecT s u m u

modifyState :: forall (m :: Type -> Type) u s. Monad m => 
               (u -> u) -> ParsecT s u m ()

updateState :: forall (m :: Type -> Type) u s. Monad m => 
               (u -> u) -> ParsecT s u m ()





-- ----------------------------------------------------------------------------
type Column :: Type
type Column = Int

type Consumed :: Type -> Type
data Consumed a = Consumed a | Empty !a

type Line :: Type
type Line = Int

type ParseError :: Type
data ParseError = Text.Parsec.Error.ParseError !SourcePos
                  [Text.Parsec.Error.Message]

type Parsec :: Type -> Type -> Type -> Type
type Parsec s u = ParsecT s u Data.Functor.Identity.Identity :: Type -> Type

type ParsecT :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype ParsecT s u m a = Text.Parsec.Prim.ParsecT
  { Text.Parsec.Prim.unParser :: forall b. State s u
  -> (a -> State s u -> ParseError -> m b)
  -> (ParseError -> m b)
  -> (a -> State s u -> ParseError -> m b)
  -> (ParseError -> m b)
  -> m b
  }

type Reply :: Type -> Type -> Type -> Type
data Reply s u a = Ok a !(State s u) ParseError | Error ParseError

type SourceName :: Type
type SourceName = String

type SourcePos :: Type
data SourcePos = Text.Parsec.Pos.SourcePos SourceName
                  {-# UNPACK #-}Line
                  {-# UNPACK #-}Column

type State :: Type -> Type -> Type
data State s u=State {stateInput :: s, statePos :: !SourcePos, stateUser :: !u}

type Stream :: Type -> (Type -> Type) -> Type -> Constraint
class Monad m => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))
  {-# MINIMAL uncons #-}
```
