# Annotation and prefix operators


(`-`) is subtraction | negation | negative literal number

*LexicalNegation* is a new extension in GHC 9.1 that detects whether the minus sign stands for negation during lexical analysis by checking for the surrounding whitespace:

```hs
a = x - y  -- subtraction
b = f -x   -- negation

f = (- x)  -- operator section
c = (-x)   -- negation

f -123 -- f (-123)
x-123  -- (-) x 123

-- Before this amendment, NegativeLiterals caused
x-123 -- to be parsed as x(-123)
```

The behavior of *NegativeLiterals* changed in GHC 9.1, and now we require that a *negative literal* must not be preceded by a **closing token**. In other words, we parse `f -123` as `f (-123)`, but `x-123` as `(-) x 123`. Before this amendment, NegativeLiterals caused `x-123` to be parsed as `x(-123)`.

About **closing tokens**: see [GHC Proposal #229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst)


---
Phrases:
infix operator
prefix annotation
bang pattern
strict pattern
lazy pattern
irrefutable pattern
strictness annotation
term-level pattern
term-level operator
term-level expression
type-level operator
type operator (symbolic name that starts with a colon)
type application (`@Int`)
as-pattern (`@(x:xs)`)


---

## Whitespace-sensitive operator parsing

https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst

- date-accepted: 2019-08-24
- implemented: GHC 9.0, but missing the warnings proposed here
- resolving ambiguities: `~, !, @, $, $$, -`

We propose to simplify GHC internals and the lexical syntax of Haskell by replacing ad-hoc parsing rules with whitespace-based disambiguation:

```hs
a ! b = <rhs>  -- Infix operator
f !a = <rhs>   -- Bang pattern
```

At the moment, Haskell has inconsistent and complicated rules to decide whether a `~` or a `!` is used as an *infix operator* or as a *prefix annotation*.

We have 6 cases to consider:
* ! as a bang pattern
* ~ as a lazy (irrefutable) pattern
* ! as a strictness annotation in type declarations
* ~ as a strictness annotation in type declarations
* ! as an infix operator
* ~ as an infix operator

GHC puts a great deal of effort into distinguishing these cases, and still does a poor job at it. Here are the rules we have as of GHC 8.8:

* In term-level patterns, `!` is considered either an infix operator or a bang pattern, depending on the module-wide `-XBangPatterns` flag.
* In term-level patterns, `~` is always considered a lazy (irrefutable) pattern.
* In term-level expressions, `!` is always considered an infix operator.
* In term-level expressions, `~` is never valid. 
* In types, `!` and `~` are considered type operators or if they have no LHS, or strictness annotations otherwise.

The consequences of these rules:

* We cannot write both of these definitions in the same module:

```hs
a ! b = <rhs>  -- Infix (!)
f !a = <rhs>   -- Bang pattern
```

* One of these definitions is valid, the other is not:

```hs
~a + ~b = <rhs>   -- Valid
!a + !b = <rhs>   -- Invalid (#1087)
```

* We cannot use ~ as a term-level operator.

In terms, we always parse `!` as an infix operator and rejig later, and it doesn't cover all possible cases.

In types, we have a hand-written state machine to detect the "no LHS" -- it is more robust but difficult to maintain.

In order to unify term-level and type-level parsers, a milestone towards *Dependent Haskell*, we will need to parse the (`~`) operator in terms, but it will further complicate parser implementation under the current arrangement.

Users typically write bang patterns and lazy (irrefutable) patterns without whitespace after it, and we can make use of this information during parsing. A similar rule is used to distinguish between `@` in type applications and as-patterns, and it works remarkably well in practice.
