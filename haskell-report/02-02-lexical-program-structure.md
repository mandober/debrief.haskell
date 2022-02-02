# 2.2 Lexical Program Structure

https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2

```js bnf
program     := { lexeme | whitespace }

lexeme      := qvarid
             | qconid
             | qvarsym
             | qconsym
             | literal
             | special
             | reservedop
             | reservedid

literal     := integer
             | float
             | char
             | string

special     := '(' | ')'    // ( ) [ ] { } ` , ;
             | '[' | ']'
             | '{' | '}'
             | '`'
             | ','
             | ';'

whitespace  := whitestuff { whitestuff }

whitestuff  := whitechar
             | comment
             | ncomment

whitechar   := newline
             | vertab
             | space
             | tab
             | uniWhite

newline     := linefeed
             | ret linefeed
             | ret
             | formfeed


linefeed    := '\n' | LF      (line feed)
ret         := '\r' | CR      (carriage ret)
formfeed    := '\f' | FF      (form feed)
vertab      := '\v' | VT      (vertical tab)
tab         := '\t' | TAB     (horizontal tab)
space       := ' '  | SP      (space)
uniWhite    := (any Unicode character defined as whitespace)

comment     := dashes [ any ⟨ symbol ⟩ { any } ] newline
dashes      := '--' { '-' }
opencom     := '{-'
closecom    := '-}'
ncomment    := opencom ANY seq { ncomment ANY seq } closecom


ANY seq     := { ANY }   ⟨ { ANY } ( opencom | closecom ) { ANY } ⟩

ANY         := graphic
             | whitechar

any         := graphic
             | space
             | tab


graphic     := small
             | large
             | symbol
             | digit
             | special
             | double_quote
             | single_quote

small       := ascSmall | uniSmall | '_'
ascSmall    := [a-z]
uniSmall    := any Unicode lowercase letter

large       := ascLarge | uniLarge
ascLarge    := [A-Z]
uniLarge    := any uppercase or titlecase Unicode letter

symbol      := ascSymbol
             | uniSymbol  ⟨ special | '_' | double_quote | single_quote ⟩

// ~ ! @ # $ % ^ & * - = + / | \ ? : . < >
ascSymbol   := '~'
             | '!'
             | '@'
             | '#'
             | '$'
             | '%'
             | '^'
             | '&'
             | '⋆'
             | '-'
             | '='  | '+'
             | '/'  | '|'
             | '\\' | '?'
             | ':'
             | '.'
             | '<'
             | '>'

uniSymbol   := (any Unicode symbol or punctuation)

digit       := ascDigit | uniDigit
ascDigit    := [0-9]
uniDigit    := any Unicode decimal digit
octit       := [0-7]
hexit       := digit | [a-fA-F]
```

Lexical analysis should use the *maximal munch* rule: at each point, the longest possible lexeme satisfying the lexeme production is read.

- although `case` is a reserved word, "cases" is not
- although `=` is reserved, `==`, `~=` and such are not

Any kind of whitespace is also a *proper delimiter* for lexemes.

Characters not in the category `ANY` are not valid in Haskell programs and should result in a lexing error.






```hs
data Program = Lexeme | Whitespace

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
  = LInteger
  | LFloat
  | LChar
  | LString

-- , ; ` | [ ] ( )
data SpecialChars = SpecialChars Char
  -- = SpecPipe
  -- | SpecComma
  -- | SpecSemicolon
  -- | SpecTick
  -- | SpecBrackets
  -- | SpecParens

data Whitespace = WhiteStuff | Whitestuff

data WhiteStuff = Whitechar | Comment | NComment

data Whitechar = Newline | Vertab | Space | Tab | UniWhite

data Newline = Return Linefeed | Return | Linefeed | Formfeed

data Return   = "CR"
data Linefeed = "LF"
data Vertab   = "VT"
data Formfeed = "FF"
data Space    = "SP"
data Tab      = "TAB"

data UniWhite -- "any Unicode character defined as whitespace"
```
