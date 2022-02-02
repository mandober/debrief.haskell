# 3.2.1 Variables

Variables
- `var` variable
- `varid` variable identifier
- `varsym` variable symbolic
- `varop` variable operator
- `qvarop` qualified variable operator


```js bnf
aexp    ::= qvar                            (or: qualified variable)
          | gcon                            (or: general constructor)
          | literal                         (or: literal)

gcon    ::= ()                              (or: unit)
          | []                              (or: list)
          | (,{,})                          (or: tuple)
          | qcon                            (or: qualified constructor)

var     ::=  varid  | (  varsym )           (variable)
qvar    ::= qvarid  | ( qvarsym )           (qualified variable)

con     ::=  conid  | (  consym )           (constructor)
qcon    ::= qconid  | ( gconsym )           (qualified constructor)

varop   ::=  varsym |  `varid`              (variable operator)
qvarop  ::= qvarsym | `qvarid`              (qualified variable operator)

conop   ::=  consym |  `conid`              (constructor operator)
qconop  ::= gconsym | `qconid`              (qualified constructor operator)

op      ::=  varop  |  conop                (operator)
qop     ::= qvarop  | qconop                (qualified operator)

gconsym ::= ':' qconsym
```
