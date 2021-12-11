# Text.Parsec.String

```hs
type Text.Parsec.String.GenParser :: Type -> Type -> Type -> Type
type Text.Parsec.String.GenParser tok st = Parsec [tok] st :: Type -> Type

type Parser :: Type -> Type
type Parser = Parsec String () :: Type -> Type

Text.Parsec.String.parseFromFile :: forall a.
  Parser a -> FilePath -> IO (Either ParseError a)
```
