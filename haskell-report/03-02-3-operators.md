# 3.2.3 Operators

Operator
- operators are functions with symbolic name applied infix
- operator symbol has symbolic name
- operator symbols are applied infix
- ordinary function has alphanumeric name
- ordinary functions are applied prefix
- backquoted ordinary identifier

In Haskell, functions can be classified broadly into two groups according to their preferred position of "activation" (within an expression):
* Alphanumeric functions prefer the prefix position
* Symbolic functions prefer the infix position

1. *Alphanumeric functions* are the ordinary functions whose name is composed according to the usual restrictions for identifier names, valid across many PLs, which can be succintly denoted using regex `[a-z_][a-zA-Z0-9'_]`. These are the functions with names like `div`, `elem`, `fmap`, etc.

2. *Symbolic functions* have their names composed using a sequance of symbols from the allowed set of characters.  These are the functions with names like `$`, `<$>`, `>>=`, etc. They are usually called operators, althought, strictly speaking, an operator is a symbolically named, binary function, used in the infix position, between the args.

Haskell provides special syntax to support infix notation.

An **operator** is a function that can be applied using *infix syntax* (Section 3.4), or a function that can be *partially applied* as a so-called *section* (Section 3.5).

An operator is either
* operator symbol (such as `+` or `$$`)
* ordinary identifier enclosed in grave accents, e.g. (3 `div` 4)

If no *fixity declaration* is given for an **infix operator** then it defaults to highest precedence and left associativity (see Section 4.4.2).

Dually, an operator symbol can be converted to an ordinary identifier by enclosing it in parentheses.

For example, `(+) x y` is equivalent to `x + y`, and `foldr (⋆) 1 xs` is equivalent to `foldr (\x y -> x ⋆ y) 1 xs`.

```hs
-- ordinary function
fmap (+3) [1,2,3]

-- ordinary function infixed
(+3) `fmap` [1,2,3]

-- operator
(+3) <$> [1,2,3]

-- operator prefixed
(<$>) (+3) [1,2,3]

(+)     -- operator section
(3 ^)   -- partial operator section
(^ 3)   -- order may be important

(`div`)
(`div` 2)
(8 `div`)
```
