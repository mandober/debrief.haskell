# Haskell :: Index :: Keywords

https://wiki.haskell.org/Keywords

Haskell keywords
- Reserved keyords
- Reserved keywords in some contexts
- Reserved symbols (compiler builtins)
- Reserved type names


## Index of keywords

https://wiki.haskell.org/Keywords

- `!`         bang, exclamation point | strictness
- `'`         single quote | to denote a `Char`, cannot be empty: 'x', '\n'
- `"`         double quote | to denote a `String`: "abc"
- `-`         dash | minus operator, sugar  for `negate`
- `--`        double dashes | line comment
- `-<`        dash+less-then | arrow
- `-<<`       dash+double LT | arrow
- `->`        dash+greater-then
- `::`        double colon
- `;`         semi-colon | code block end
- `<-`        LT+dash | "slurp"
- `,`         comma | listing members
- `=`         equal sign | equations
- `=>`        equals+gt | constraints
- `>`         gt | 
- `?`         question mark | 
- `#`         hash | prim-ops, prim-kind
- `*`         asterisk | mult
- `@`         at | as-patterns
- `[|`,`|]`   bracket-pipe pairs | Template Haskell
- `\`         backslash | escape char
- `_`         underscope | hole
- (tick)      tick | using a function as an infix op, a `mod` b
- `{`, `}`    braces | denote code blocks in non-layout syntax
- `{-`, `-}`  brace-dash pairs | block comments
- `|`
- `~`
- as
- case, of
- class
- data
- data family
- data instance
- default
- deriving
- deriving instance
- do
- forall
- foreign
- hiding
- if, then, else
- import
- infix, infixl, infixr
- instance
- let, in
- mdo
- module
- newtype
- proc
- qualified
- rec
- type
- type family
- type instance
- where


## Haskell keywords

Haskell keywords
- Reserved keyords
- Reserved keywords in some contexts
- Reserved symbols (compiler builtins)
- Reserved type names

## Reserved Haskell keywords as Strings

```hs
keywords =
  ["case","class","data","default","deriving","do","else","forall"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","qualified","then","type","where","_"
  ,"foreign","ccall","as","safe","unsafe"]

keyglyphs =
  ["..","::","=","\\","|","<-","->","@","~","=>","[","]"]

layoutchars =
  map (:[]) ";{}(),"

symbols =
  "!#$%&*+./<=>?@\\^|-~"
```

You can just

```hs
import Language.Haskell.HsColour.Classify

isKeyword :: String -> Bool
isKeyword = (== [Keyword]) . map fst . tokenise

-- it contains TokenType
data TokenType
  = Space
  | Keyword
  | Keyglyph
  | Layout
  | Comment
  | Conid
  | Varid
  | Conop
  | Varop
  | String
  | Char
  | Number
  | Cpp
  | Error
  | Definition

tokenise :: String -> [(TokenType, String)]

>>> tokenise "::"       -- [(Keyglyph,"::")]
>>> tokenise ":"        -- [(Conop,":")]
>>> tokenise "forM"     -- [(Definition,"forM")]
>>> tokenise ">>="      -- [(Varop,">>=")]
>>> tokenise "|>+<>-+"  -- [(Varop,"|>+<>-+")]
>>> tokenise "<bas>"    -- [(Varop,"<"),(Varid,"bas"),(Varop,">")]
>>> tokenise "11_1111"  -- [(Number,"11"),(Error,"_1111")]
>>> tokenise "#def"     -- [(Cpp,"#"),(Varid,"def")]
>>> tokenise ";"        -- [(Layout,";")]
>>> tokenise "  "       -- [(Space,"  ")]
```


## Ref

https://github.com/AnanthaRajuC/Reserved-Key-Words-list-of-various-programming-languages/blob/master/language-files/Haskell%20%20Reserved%20Words.md

https://github.com/AnanthaRajuC/Reserved-Key-Words-list-of-various-programming-languages/
