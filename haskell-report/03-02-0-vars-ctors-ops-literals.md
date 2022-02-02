# 3.2 Variables, Constructors, Operators and Literals

https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-240003.2


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
