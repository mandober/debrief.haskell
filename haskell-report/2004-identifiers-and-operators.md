# 2.4 Identifiers and Operators

https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4

## Identifiers

```js bnf
varid      := ( small { small | large | digit | prime } )   ⟨ reservedid ⟩

conid      := large {small | large | digit | prime }

reservedid := '_'
            | 'module'
            | 'import'
            | 'type'
            | 'newtype'
            | 'data'
            | 'class'
            | 'default'
            | 'deriving'
            | 'do'
            | 'foreign'
            | 'instance'
            | 'let'
            | 'in'
            | 'where'
            | 'infix' | 'infixl' | 'infixr'
            | 'if'    | 'then'   | 'else'
            | 'case'  | 'of'

            | as, via, stock, ?
```


An *identifier* consists of a letter followed by zero or more letters, digits, underscores, and single quotes.

Identifiers are lexically distinguished into two namespaces:
- those that begin with a lowercase letter: *variable identifiers*
- those that begin with an upper-case letter: *constructor identifiers*

## Underscore

- Underscore is treated as a lowercase letter, and can occur wherever a lowercase letter can.
- However, underscore alone by itself is a *reserved identifier*, used as wild card in patterns.
- Compilers that offer warnings for unused identifiers are encouraged to suppress such warnings for identifiers beginning with underscore. This allows programmers to use `_foo` for a parameter name they expect to be unused.

## Operator symbols

[tbl]

*Operator symbols* are formed from one or more *symbol characters*, as defined above, and are lexically distinguished into two namespaces:
- starting with a colon is a constructor
- starting with any other character is an ordinary identifier

A colon by itself, `:`, is reserved for use as the list constructor; this makes its treatment uniform with other parts of list syntax, such as `[]` and `[a,b]`.

Other than the *special syntax for prefix negation*, all operators are *infix*, although each infix operator can be used in a section to yield partially applied operators.

All of the standard infix operators are just predefined symbols and may be rebound.

## Names

In the remainder of the report 6 different kinds of names will be used:

[tbl]

- variables and type variables are represented by small-case identifiers
- others by identifiers beginning with capital-case
- variables and constructors have infix forms, the other 4 do not
- Module names are a dot-separated sequence of conids

## Qualified names

A name may optionally be qualified in certain circumstances by prepending them with a *module identifier*.

These names may be qualified:
- variable
- constructor
- type constructor
- type class names

Type variables or module names cannot be name-qualified.

Qualified names are discussed in detail in Chapter 5.

[tbl]

> Since a qualified name is a lexeme, no spaces are allowed between the qualifier and the name.

Sample lexical analyses:

This | Lexes as | Tokens | Meaning
-----|----------|--------|----------------
f.g  | f . g    | 3      | composition
F.g  | F.g      | 1      | qualified 'g'
f..  | f ..     | 2      | (range?) [f .. g] ?
F..  | F..      | 1      | qualified '.'
F.   | F .      | 2      | (composition?)
M.+  | M.+      | 1      | qualified infix op (+), e.g. `3 P.+ 4 == 7`

The qualifier does not change the syntactic treatment of a name; for example, `Prelude.+` is an infix operator with the same fixity as the definition of `+` in the Prelude (Section 4.4.2).

Using *qualified infix operators* in infix position looks ugly, and using it in prefix position requires parens around it:

```hs
s1 = 3 Prelude.+ 5
s2 = (Prelude.+) 3 5
```

The workaround could be to import the infix operator directly from a module before importing that module aliased.

```hs
import Data.Set ((\\))
import Data.Set qualified as S

s1 = a \\ b
```
