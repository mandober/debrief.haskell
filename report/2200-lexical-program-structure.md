# 2.2 Lexical Program Structure

https://www.haskell.org/onlinereport/haskell2010/haskellch2.html

```hs
data Program
  = Lexeme
  | Whitespace

data Lexeme
  = QVarId
  | QConId
  | QVarSym
  | QConSym
  | Literal
  | Special
  | ReservedOp
  | ReservedId

data Literal
  = Integer
  | Float
  | Char
  | String

-- , ; ` | [ ] ( )
data Special
  = ❱|❰        (pipe)
  | ❱,❰        (comma)
  | ❱;❰        (semicolon)
  | ❱`❰        (tick)
  | ❱[❰ | ❱]❰  (brackets)
  | ❱{❰ | ❱}❰  (parens)


data Whitespace = WhiteStuff { Whitestuff }

data WhiteStuff
  = Whitechar
  | Comment
  | NComment

data Whitechar
  = Newline
  | Vertab
  | Space
  | Tab
  | Uniwhite

data Newline
  = Return Linefeed
  | Return
  | Linefeed
  | Formfeed

data Return   = a carriage return
data Linefeed = a line feed
data Vertab   = a vertical tab
data Formfeed = a form feed
data Space    = a space
data Tab      = a horizontal tab

data UniWhite = "any Unicode character defined as whitespace"
```


Lexical analysis should use the "maximal munch" rule: at each point, the longest possible lexeme satisfying the lexeme production is read. So, although `case` is a reserved word, "cases" is not; and although `=` is reserved, `==` and `~=` are not.

Any kind of whitespace is also a proper delimiter for lexemes.

Characters not in the category ANY are not valid in Haskell programs and should result in a lexing error.
