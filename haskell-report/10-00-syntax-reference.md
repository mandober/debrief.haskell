# 10. Syntax Reference
https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010

## 10.1 Notational Conventions
These notational conventions for presenting syntax are in [2.1 Notational Conventions](02-00-lexical-structure.md#21-notational-conventions).

## 10.2 Lexical Syntax

- Pipe symbol `|` is presented as `¦` because meta symbol for alternation is `|`
- Meta symbols: `|`, `{ }`, `=`, `[]`
- optional X is denoted in braces as `{ X }`

```
program = { lexeme | whitespace }

lexeme
  = qvarid
  | qconid
  | qvarsym
  | qconsym
  | literal
  | special
  | reservedop
  | reservedid

literal = integer | float | char | string

special
  = ¦
  | ,
  | ;
  | [
  | ]
  | `
  | {
  | }

whitespace = whitestuff { whitestuff }

whitestuff
  = whitechar
  | comment
  | ncomment

whitechar
  = newline
  | vertab
  | space
  | tab
  | uniWhite

newline
  = return linefeed
  | return
  | linefeed
  | formfeed

  return    = a carriage return
  linefeed  = a line feed
  vertab    = a vertical tab
  formfeed  = a form feed
  space     = a space
  tab       = a horizontal tab
  uniWhite  = any Unicode character defined as whitespace

  comment  = dashes [ any⟨symbol⟩ {any} ] newline
  dashes   = -- {-}
  opencom  = {-
  closecom = -}
  ncomment = opencom ANY seq {ncomment ANY seq} closecom

ANY seq  = {ANY }⟨{ANY } ( opencom | closecom ) {ANY }⟩
  ANY      = graphic | whitechar
  any      = graphic | space | tab
  graphic  = small | large | symbol | digit | special | " | '
  small    = ascSmall | uniSmall | _
  uniSmall = any Unicode lowercase letter
  ascSmall = a | b | … | z

  large    = ascLarge | uniLarge
  ascLarge = A | B | … | Z
  uniLarge = any uppercase or titlecase Unicode letter
  symbol   = ascSymbol | uniSymbol⟨special | _ | " | '⟩

  ascSymbol = ! | # | $ | % | & | ⋆
            | + | . | / | < | = | > | ?
            | @ | \ | ^ | | | - | ~ | :

  uniSymbol = any Unicode symbol or punctuation
  digit     = ascDigit | uniDigit
  ascDigit  = 0 | 1 | … | 9
  uniDigit  = any Unicode decimal digit
  octit     = 0 | 1 | … | 7
  hexit     = digit | A | … | F | a | … | f


varid = (small {small | large | digit | ' })⟨reservedid⟩

conid = large {small | large | digit | ' }

reservedid
  = case
  | of
  | if
  | then
  | else
  | let
  | in
  | do
  | module
  | where
  | import
  | data
  | newtype
  | type
  | class
  | instance
  | deriving
  | infix
  | infixl
  | infixr
  | default
  | foreign
  | _

varsym = ( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩

consym = ( : {symbol})⟨reservedop⟩

reservedop
  = @       (1-char-op)
  | :
  | ~
  | =
  | \
  | ¦
  | ..      (2-chars-op)
  | ::
  | =>
  | <-
  | ->


varid             (variables)
conid             (constructors)
tyvar = varid     (type variables)
tycon = conid     (type constructors)
tycls = conid     (type classes)
modid = { conid . } conid (modules)

qvarid  = [ modid . ] varid
qconid  = [ modid . ] conid
qtycon  = [ modid . ] tycon
qtycls  = [ modid . ] tycls
qvarsym = [ modid . ] varsym
qconsym = [ modid . ] consym


integer
  = decimal
  | 0o octal
  | 0O octal
  | 0x hexadecimal
  | 0X hexadecimal float = decimal . decimal [exponent]
  | decimal exponent exponent = (e | E) [+ | -] decimal


  decimal     = digit{digit}
  octal       = octit{octit}
  hexadecimal = hexit{hexit}

  char    = ' (graphic⟨' | \⟩ | space | escape⟨\&⟩) '

  string  = " {graphic⟨" | \⟩ | space | escape | gap} "

  escape  = \ ( charesc | ascii | decimal | o octal | x hexadecimal )

  charesc = a | b | f | n | r | t | v | \ | " | ' | &

  ascii = ^cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
                | BEL | BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI
                | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
                | EM  | SUB | ESC | FS  | GS  | RS  | US  | SP  | DEL

  cntrl = ascLarge | @ | [ | \ | ] | ^ | _
  gap   = \ whitechar {whitechar} \
```
