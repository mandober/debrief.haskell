# List of symbols

Symbol Names
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names

Since Haskell allows many symbolic names that C compilers or assembly might not allow (`:`, `%`, `#`) these have to be encoded using z-encoding.

One consequence of the z-encoding is that alphabetic chars 'z' and 'Z' must be escaped, by doubling, as 'zz' and 'ZZ'.

See [compiler/GHC/Utils/Encoding.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Utils/Encoding.hs) and [$1535](https://gitlab.haskell.org/ghc/ghc/snippets/1535) for encoding/decoding implementations.



Decoded | Encoded   | Note
--------|-----------|------------
◪ ◪ ◪ ◪ | ◪ ◪ ◪ ◪ ◪ | ◪ **TUPLES**
()      | Z0T       | unit
*n/a*   |*n/a*      | no Z1T
(,)     | Z2T       | pair
(,,)    | Z3T       | 3-tuple
(,,,)   | Z4T       | 4-tuple
◪ ◪ ◪ ◪ | ◪ ◪ ◪ ◪ ◪ | ◪ **UNBOXED TUPLES**
*n/a*   |*n/a*      | no Z0H 
(# #)   | Z1H       | unboxed 1-tuple
(#,#)   | Z2H       | unboxed 2-tuple
(#,,#)  | Z3H       | unboxed 3-tuple
◪ ◪ ◪ ◪ | ◪ ◪ ◪ ◪ ◪ | ◪ **CTOR CHARS**
`(`     | ZL        | Left
`)`     | ZR        | Right
`[`     | ZM        | 'M' before 'N' in []
`]`     | ZN        | 'N' after 'M' in []
`:`     | ZC        | Colon
◪ ◪ ◪ ◪ | ◪ ◪ ◪ ◪ ◪ | ◪ **CHARS**
`&`     | za        | Ampersand
`|`     | zb        | Bar
`^`     | zc        | Caret
`$`     | zd        | Dollar
`=`     | ze        | Equals
`>`     | zg        | Greater than
`#`     | zh        | Hash
`.`     | zi        | The dot of the 'i'
`<`     | zl        | Less than
`-`     | zm        | Minus
`!`     | zn        | Not
`+`     | zp        | Plus
`'`     | zq        | Quote
`\`     | zr        | Reverse slash
`/`     | zs        | Slash
`*`     | zt        | Times sign
`_`     | zu        | Underscore
`%`     | zv        |
z, Z    | zz, ZZ    | must be escaped


Any other character not in the list below is encoded as a 'z' followed by its hex code (lower case, variable length) followed by 'U'. If the hex code starts with 'a', 'b, 'c', 'd', 'e' or 'f', then an extra '0' is placed before the hex code to avoid conflicts with the other escape characters.

Before      | After
------------|----------
Trak        | Trak
foo_wib     | foozuwib
`>`         | zg
`>1`        | zg1
foo#        | foozh
foo##       | foozhzh
foo##1      | foozhzh1
fooZ        | fooZZ
:+          | ZCzp
()          | Z0T
(,,,,)      | Z5T
(# #)       | Z1H
(#,,,,#)    | Z5H
