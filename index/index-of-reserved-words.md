# Haskell :: Index :: Reserved words

https://wiki.haskell.org/Keywords

## Reserved keyords

- Reserved keyords (always)
  - module
  - import
  - type
  - newtype
  - data
  - class
  - instance
  - deriving
  - default
  - do
  - case
  - of
  - let
  - in
  - where
  - infix, infixl, infixr
  - if
  - then
  - else
- Reserved keywords (in some contexts)
  - `as`          in import statement
  - `qualified`   in import statement
  - `hiding`      in import statement
  - `forall`      at type level
  - `foreign`     with FFI extension
- Reserved symbols (compiler builtins)
  - `_`     reserved identifier
  - `::`    type annotation
  - `->`    function type ctor
  - `=>`    context
  - `<-`    do notation monadic binding, the "slurp" operator
  - `\`     lambda abstraction symbol, `λa.a` == `\ a -> a`
  - `[]`    list type ctor, `[] a == [a]`
            empty list data ctor
  - `:`     cons data ctor for lists, `(:) x xs == x : xs`
  - `()`    unit type and data ctor
            grouping, precedence
  - `,`     tuple(s) type and data ctor(s)
  - `(,)`   tuple ctors, `(,) a b == (a, b)`
  - `(,,)`  tuple ctors, `(,,) a b c == (a, b, c)`
- Type names
  - Bool
  - Ordering
  - Char
  - String
  - Int
  - Integer
  - Float
  - Double
  - Fractional


## TOC

<!-- TOC -->

- [Reserved keyords](#reserved-keyords)
- [TOC](#toc)
- [bang](#bang)
- [Bang patterns](#bang-patterns)
- [single quote](#single-quote)
- [double single quote](#double-single-quote)
- [dash](#dash)
- [double dash](#double-dash)
- [headless arrows](#headless-arrows)
- [arrow](#arrow)
- [Double colon](#double-colon)
- [Semicolon](#semicolon)
- [Leftward arrow](#leftward-arrow)
- [Comma](#comma)
- [Equals](#equals)
- [Fat arrow](#fat-arrow)
- [GT](#gt)
- [Question mark](#question-mark)
- [Hash](#hash)
- [Asterisk](#asterisk)
- [At](#at)
- [TH quotes](#th-quotes)
- [Backslash](#backslash)
- [Underscore](#underscore)
- [Backtick](#backtick)
- [Braces](#braces)
- [Block comment](#block-comment)
- [Pipe](#pipe)
- [Tilde](#tilde)
- [as](#as)
- [case-of](#case-of)
- [class](#class)
- [data](#data)
- [Data family](#data-family)
- [Data instance](#data-instance)
- [default](#default)
- [deriving](#deriving)
- [Deriving instance](#deriving-instance)
- [do](#do)
- [forall](#forall)
- [foreign](#foreign)
- [hiding](#hiding)
- [if-then-else](#if-then-else)
- [Imports](#imports)
- [Fixity declarations](#fixity-declarations)
- [instance](#instance)
- [let-in](#let-in)
- [mdo](#mdo)
- [module](#module)
- [newtype](#newtype)
- [proc](#proc)
- [qualified](#qualified)
- [rec](#rec)
- [type](#type)
- [Type family](#type-family)
- [type instance](#type-instance)
- [where](#where)
- [BNF for Haskell identifiers and operators](#bnf-for-haskell-identifiers-and-operators)
- [References](#references)

<!-- /TOC -->


## bang

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BangPatterns

1. Whenever a data ctor is applied, each argument to the constructor is evaluated iff the corresponding type in the algebraic datatype declaration has a strictness flag, denoted by a bang (exclamation point).
2. The bang (!) is also used in the `BangPatterns` extension, to indicate strictness in patterns.


```hs
data STList a
    = STNil
    | STCons a !(STList a)
--             ^^^^^^^^^^^
-- the second arg to STCons will be evaluated prior to STCons is applied


-- Illustrating the difference between STRICT VS LAZY CTOR APPL:
stList = STCons 1 undefined
lzList = (:)    1 undefined

-- evaluates to undefined when applied to stList
stHead (STCons h _) = h

-- evaluates to 1 when applied to lzList
lzHead (h : _)      = h
```

## Bang patterns

The bang is also used in `BangPatterns` extension to enforce strictness of a pattern in pattern-matching:

```hs
-- first evals exp to WHNF then matches the results
f !pat = exp


-- makes f strict in x
f !x = True
-- without it, f would be lazy
f x = True


-- f is strict in x, but not in y
f (!x, y) = [x,y]

-- f is strict in both x and y
f (!x, !y) = [x,y]


-- I
-- bang only really has effect if it precedes a var or wild-card pattern:
f !(x,y) = [x,y]
g  (x,y) = [x,y]
-- here, f and g are identical: bang placed before a
-- pattern that forces the evaluation anyway is a NOOP


-- II
-- Bang in let/where makes the binding strict:
let !x     = e in body
let !(p,q) = e in body
-- e is evaluated before starting to evaluate `body`

-- IIa
-- Nested bangs in let/where pattern binding behave
-- uniformly to all other forms of pattern matching:
let (!x, [y]) = e in body

-- is equivalent to:
let { t = case e of (x,[y]) -> x `seq` (x,y)
      x = fst t
      y = snd t }
in body
-- binding is lazy, but when either x or y is evaluated, by `body` the
-- entire pattern is already matched (including the forced eval of x). NOOP


-- III
-- Bang patterns in case expressions:
g5 x = let y = f x in body
g6 x = case f x of {  y -> body }
g7 x = case f x of { !y -> body }
-- g5 and g6 are the same
-- g7 evaluates `f x` binding the result to y, then evaluates body

-- III
-- ambiguity with bangs:
f !x = 3
-- GHC interprets this as the definition of f with a bang arg
-- not as the def of the infix operator, (!):
f ! x = 3
-- to define an infix op with enabled bang patterns, use prefix form:
(!) f x = 3
```


Matching an expression `e` against a pattern `!p` is done by first evaluating `e` (to WHNF) and then matching the result against `p`.

* Semantics of let bindings with bang patterns:
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#recursive-and-polymorphic-let-bindings

* A pattern with a bang at the outermost level is not allowed at the top level of a module.

* In a rare case, when bang-patterns are enabled, and you want to define the infix operator (!), you must do it using the prefix notation: `(!) f x = 3`. Otherwise, GHC cannot tell if it's the definition of the infix function (`!`) or an arg with a bang pattern, so it defaults to the bang pattern (as it should; whoever wants to define an infix op named `!` is a banger).


## single quote

Single quote is used
1. to denote character literals, `'a'`
2. in Template Haskell, as the name of a variable or data ctor: `'x`, `'Left`
3. to denote the promoted data ctor: `'True`

## double single quote

Doubling the single quote is used in Template Haskell, as the name of a type ctor or class: `''Int`, `''Either`, `''Show`

## dash

1. negation function, `(-) = negate`
2. negative number sign

The dash operator is a *magic or irregular token* cos it has ambiguous parsing. Depending on the whitespace, it may be parsed as the negative integer, `(-1)`, or as a section, `(- 1)`.

Dash or minus sign is also sugar for `negate` function. The expression `2 - 1` desugars to:

    negate (fromInteger 2) (fromInteger 1)

Before, in order to denote its use as a section we had to use the form `(+(-1))` or the `subtract` function. Now, with some pragmas enabled, the whitespace makes decisions about what token it is, i.e. no space `-1` for negation, spacing required to be recognized as 'subtract', `a - 1`, a section is `(- 1)` not `(-1)`, etc.

>Use gratuitous amounts of whitespace, squeeze them tokens not.


## double dash

Double dash starts a single-line comment, unless immediately followed by an allowed character (other than the dash), in which case it is the symbolic name for an operator.

```hs
-- this is a comment
--also a comment
---this too
main = print "hello world"

foo --+ bar
--  ^^^ interpreted as an operator
```

## headless arrows

These arrows are used in proc notation: `-<`, `-<<`

## arrow

Arrow symbol (->) is used as
1. function type ctor
2. Instead of "dot" in lambda abstractions
3. To denote alternatives in `case` statements
4. To denote alternatives in `LambdaCase`, `\case -> …`
5. To denote alternatives in `MultiWayIf`
6. To express functional dependencies, `class Add a b c | a b -> c where`
7. In kind functions
8. In view patterns

```hs
-- [1] Function type ctor
--     ==================

-- If it could be defined the fn type ctor would look like
-- (what would its data ctor be like?)
data (->) a b = …
data (->) a b = a -> b
data (~~>) a b = (:◕‿◕:) a b

type Fn r a = Fn (a -> r)

-- Function type ctor occurs in almost every sig
--       ↓    ↓      ↓    ↓      ↓         ↓
es :: (a -> b -> c) -> (d -> b) -> (a, d) -> c

-- [2] Instead of the "dot" in lambda abstractions
es f g = \ (x, y) -> f x (g y)


-- [3] To denote alternatives in `case` statements
case x of
  Nothing -> False
  Just x  -> True

case x of Nothing -> False; Just x  -> True

-- [4] To denote alternatives in `LambdaCase`
case 1 -> 0
_      -> 1

-- [5] To denote alternatives in `MultiWayIf`
if | 1 == 0    -> 1
   | 1 == 2    -> 2 
   | otherwise -> 3

-- [6] To express functional dependencies
-- This assumes that each type 'c' can contain only one
-- type, i.e. type 'c' uniquely determines type 'elt'
class Contains c elt | c -> elt where

-- [7] In kind functions
ghci> :kind (->)
(->) :: * -> * -> *

-- [8] In view patterns
```


## Double colon

The double colon, `::`, is used as
1. a type judgement
2. a kind judgement

It is a non-standard notation for the type judgement, which is normally a single colon (in type theory), `α : τ`. In similar languages, that respect the standard denotation, the double colon is used as the list cons ctor. However, designers of Haskell decided that it should be the other way around.

```hs
-- type judgement
id :: a -> a

-- kind judgement
(->) :: * -> * -> *

-- cons operator
x : xs
```

## Semicolon

- semicolon, `;`, is used to separate statements when layout is not used.

## Leftward arrow

The leftward arrow, `<-`, is used
1. in do-notation, as the "slurp" or "draw from" operator
2. in list comprehension generators, as "draw from" or "in" operator
3. in pattern guards, as the "matches" operator

```hs
-- (1)
do x <- getChar
   putChar x

-- (2)
[ (x,y) | x <- [1..10], y <- ['a'..'z'] ]

-- (3)
f x y | Just z <- g x = True
      | otherwise     = False
```

## Comma

Comma, `,` is used as
1. separator in lists, tuples, records.
2. In list comprehensions before generators, "and"
3. In list comprehensions before Boolean tests, "when"
4. In guards inside case expressions, "and when"
5. In module's import and export lists

```hs
-- (1)
[1,2,3]
(1,2,3)
Point {x = 1, y = 2}

-- (2)
[ (x,y) | x <- [1..10], y <- ['a'..'z'], x > 42 ]

-- (3)
[ (x,y) | x <- [1..10], y <- ['a'..'z'], x > 42 ]

-- (4)
case [1,3,9] of xs | (x:ys) <- xs, (y:_) <- ys, let z=x+1, z /= y -> [x,y,z]

-- (5)
module MyModule (MyData (C1,C2), myFun) where
import MyModule (MyData (C1,C2), myFun)
```

## Equals

Equals, `=`, is used
1. in definitions
2. in pattern-matching records

```hs
--- (2)
case point of
  Point {x = x0, y = y0} -> f x0 y0
```

## Fat arrow

Fat arrow, `=>`, is used
1. to indicate instance contexts

```hs
sort :: Ord a => [a] -> [a]
```

## GT

GT, `>`, is used
1. as an operator
2. in the Bird's style Literate Haskell file, used to introduce a code line

## Question mark

Question mark, `?`, is used
1. to denote an implicit parameter

```hs
ghci> :t ?foo ++ "bar"
?foo ++ "bar" :: (?foo::[Char]) => [Char]
```

## Hash

Hash, `#`, is used
1. as `MagicHash`

## Asterisk

Asterisk, `*`, is used
1. as an ordinary operator name on the value level
2. on the kind level, the kind of inhabited types (deprecated by `Type`)

## At

At, `@`, is used
1. in patterns of the form `var@pat`, which are as-patterns, and allow one to use `var` as the name for the value being matched by `pat`
2. in visible type applications

```hs
case e of { xs@(x:rest) -> if x == 0 then rest else xs }
-- is equivalent to:
let { xs = e } in case xs of { (x:rest) -> if x == 0 then rest else xs }
```

## TH quotes

Template Haskell quotes: open, `[|`, and closing, `|]` are used
1. expression quotation:   `[| print 1 |]`
2. declaration quotation:  `[d| main = print 1 |]`
3. type quotation:         `[t| Either Int () |]`
4. pattern quotation:      `[p| (x,y) |]`
5. quasiquotation:         `[nameOfQuasiQuoter| ... |]`

## Backslash

The backslash, `` is used
1. as an escape character in multiline strings
2. to denote the lambda binder in lambda functions, ` x y -> …`

## Underscore

The underscore, `_` is used
1. as the "nevermind" pattern `_` (it is as if an identifier not used elsewhere were put in its place), but it doesn't bound an arg.
2. to mark the "term holes" at the term level
3. to mark the "type holes" at the type level

```hs
-- (1)
case e of { [x,_,_]  ->  if x==0 then True else False }

-- (3)
foo :: (a -> b) -> b -> [_] -> [b]
```

## Backtick

Backticks are used
1. to enclose a function so it can be used as an infix operator

```hs
x `div` y
```


## Braces

A pair of braces denotes
1. braces denote an explicit block, `{ }`, with the `;` separated statements
2. record update notation

```hs
-- (2)
changePrice :: Thing -> Price -> Thing
changePrice x new = x { price = new }
```

## Block comment

Block comments: opening, `{-`, and closing, `-}`
1. everything between `{-` followed by a space and `-}` is a block comment

## Pipe

The pipe symbol, `|`, is used
1. in data definitions, as the sum type
2. in list comprehensions it acts as "such that"
3. in guards as "when"
4. in functional dependencies as "where"

```hs
-- (1)
data Maybe a = Just a | Nothing

-- (2)
squares = [a*a | a <- [1..]]

-- (3)
safeTail x | null x    = []
           | otherwise = tail x

-- (4)
class Contains c elt | c -> elt where
```

## Tilde

The tilde symbol, `~`, is used
1. in lazy pattern bindings
2. as equality constraints in contexts to assert that two types are the same


```hs
-- (1)
-- Matching the pattern `~pat` against a value always succeeds, and matching
-- will only diverge when one of the variables bound in the pattern is used.
f1, f2 :: Maybe Int -> String
f1 x = case x of   Just n  -> "Got it"
f2 x = case x of ~(Just n) -> "Got it"

(+++), (++++) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d) 
(f +++ g) ~(x, y) = (f x, g y)
(f ++++ g) (x, y) = (f x, g y)
-- Then we have
f1 Nothing                        -- Exception: Non-exhaustive patterns in case
f2 Nothing                        -- "Got it"
(const 1 +++ const 2) undefined   -- (1,2)
(const 1 ++++ const 2) undefined  -- Exception: Prelude.undefined

-- (2)
example :: (F a ~ b) => a -> b
```

In (2), the type `F a` must be the same as the type `b`, which allows one to constrain polymorphism (especially where type families are involved), but to a lesser extent than fundep.

## as

The keyword `as` is used
1. to rename module imports


```hs
import qualified Data.Map as M
main = print (M.empty :: M.Map Int ())
```

Like *qualified* and *hiding*, *as* is **not a reserved word**, but may be used as function or variable name.

## case-of

A `case…of` expression has the general form

```hs
case e of { p₁ match₁ ; … ; pₙ matchₙ }
```

where each <matchᵢ> is of the general form

```hs
  | g₁ -> e₁
    …
  | gₘ -> eₘ
  where
    decls
```

Each alternative consists of patterns <pᵢ> and their matches <matchᵢ>.

Each <matchᵢ>, in turn, consists of a sequence of pairs of guards <gᵢⱼ> and bodies <eᵢⱼ> (expressions), followed by optional bindings <declsᵢ> that scope over all of the guards and expressions of the alternative.

An alternative of the form, is treated as shorthand for:

```hs
  pat | True -> exp
    where decls
```

A case expression must have at least one alternative 
and each alternative must have at least one body.

Each body must have the same type, and 
the type of the whole expression is that type.

A case expression is evaluated 
by pattern matching the expression `e` 
against the individual alternatives. 
The alternatives are tried sequentially, 
from top to bottom.

If `e` matches the pattern in the alternative, 
the guards for that alternative are tried 
sequentially from top to bottom 
in the environment of the `case` expression which is extended
- first by the bindings created during the matching of the pattern, and
- then by the <declsᵢ> in the `where` clause associated with that alternative

If one of the guards evaluates to True, 
the corresponding rhs is evaluated 
in the same environment as the guard.

If all the guards evaluate to False, 
matching continues with the next alternative.

If no match succeeds, the result is bottom.

## class

A class declaration introduces a new type class and the overloaded methods that must be supported by any type that is an instance of that class.

```hs
class Num a  where
    (+)    :: a -> a -> a
    negate :: a -> a
```

## data

The data declaration is how one introduces new ADT into Haskell.

```hs
data Set a = NilSet | ConsSet a (Set a)
```

Another example, to create a datatype to hold an abstract syntax tree:

```hs
data Exp = Ebin   Operator Exp Exp 
         | Eunary Operator Exp 
         | Efun   FunctionIdentifier [Exp] 
         | Eid    SimpleIdentifier
```

## Data family

Declares a datatype family.

## Data instance

Declares a datatype family instance.

## default

Ambiguities in the class Num are most common, so Haskell provides a way to resolve them - with a `default` declaration.

Only one default declaration is permitted per module, and its effect is limited to that module. If no default declaration is given in a module then it assumed to be: `default (Integer, Double)`

## deriving

The data and newtype declarations contain an optional deriving form. If the form is included, then derived instance declarations are automatically generated for the datatype in each of the named classes.

Derived instances provide convenient commonly-used operations for user-defined datatypes. For example, derived instances for datatypes in the class Eq define the operations == and /=, freeing the programmer from the need to define them.

```hs
data T = A | B | C deriving (Eq, Ord, Show)
```

In the case of newtypes, GHC extends this mechanism to Cunning Newtype Deriving.

## Deriving instance

`StandaloneDeriving` language extension

```hs
{-# LANGUAGE StandaloneDeriving #-}

data A = A

deriving instance Show A
```

## do

Syntactic sugar for use with monadic expressions.

```hs
do { x ; result <- y ; foo result }

-- is shorthand for
x >>
y >>= result ->
foo result
```

## forall

This is a GHC extension, and the only a reserved word within types.

Type variables in a Haskell type expression are all assumed to be universally quantified; there is no explicit syntax for universal quantification, in standard Haskell 98/2010.

For example, the type expression `a -> a` denotes the type `forall a. a -> a`.

For clarity, however, we often write quantification explicitly when discussing the types of Haskell programs. When we write an explicitly quantified type, the scope of the forall extends as far to the right as possible.

GHC introduces the `forall` keyword, allowing explicit quantification, e.g. to encode **existential types**.

```hs
data Foo = forall a. MkFoo a (a -> Bool) | Nil

MkFoo :: forall a. a -> (a -> Bool) -> Foo
Nil   :: Foo

[MkFoo 3 even, MkFoo 'c' isUpper] :: [Foo]
```

## foreign

A keyword for the FFI that introduces either a `foreign import` declaration, which makes a function from a non-Haskell library available in a Haskell program, or a `foreign export` declaration, which allows a function from a Haskell module to be called in non-Haskell contexts.

## hiding

When importing modules, without introducing a name into scope, entities can be excluded by using the form which specifies that all entities exported by the named module should be imported except for those named in the list.

```hs
hiding (import1 , ... , importn )

import Prelude hiding (lookup,filter,foldr,foldl,null,map)
```

## if-then-else

A conditional expression has the form `if e1 then e2 else e3`, and returns the value of `e2` if the value of `e1` is True, `e3` if `e1` is False, and bottom otherwise.

```hs
max a b = if a > b then a else b
```

## Imports

Modules may reference other modules via explicit import declarations, each giving the name of a module to be imported and specifying its entities to be imported.

```hs
module Main where
  import A
  import B
  main = A.f >> B.f

module A where
  f = ...

module B where
  f = ...
```

## Fixity declarations

The 3 fixity declarations (`infix`, `infixl`, `infixr`) gives the fixity and binding precedence of one or more operators. The integer in a fixity declaration must be in the range 0 to 9. A fixity declaration may appear anywhere that a type signature appears and, like a type signature, declares a property of a particular operator.

There are three kinds of fixity, non-, left- and right-associativity (infix, infixl, and infixr, respectively), and ten precedence levels, 0 to 9 inclusive (level 0 binds least tightly, and level 9 binds most tightly).

## instance

An instance declaration declares that a type is an instance of a class and includes the definitions of the overloaded operations - called class methods - instantiated on the named type.

```hs
instance Num Int  where
    x + y       =  addInt x y
    negate x    =  negateInt x
```

## let-in

Let expressions have the general form: `let { d1 ; ... ; dn } in e`

They introduce a nested, lexically-scoped, mutually-recursive list of declarations (let is often called let rec in other languages). The scope of the declarations is the expression e and the right hand side of the declarations.

Within `do`-blocks or list comprehensions `let { d1 ; ... ; dn }` without `in` serves to introduce local bindings.

## mdo

The recursive `do` keyword.

## module

Technically speaking, a module is really just one big declaration which begins with the keyword module.

```hs
module Tree ( Tree(Leaf,Branch), fringe ) where

data Tree a = Leaf a | Branch (Tree a) (Tree a) 

fringe :: Tree a -> [a]
fringe (Leaf x)            = [x]
fringe (Branch left right) = fringe left ++ fringe right
```

## newtype

The `newtype` declaration is how one introduces a renaming for an algebraic data type into Haskell. This is different from `type` below, as a `newtype` requires a new ctor as well. As an example, when writing a compiler one sometimes further qualifies `Identifier`s to assist in type safety checks:

newtype SimpleIdentifier = SimpleIdentifier Identifier
newtype FunctionIdentifier = FunctionIdentifier Identifier

Most often, one supplies smart constructors and destructors for these to ease working with them.

See the page on types for more information, links and examples.

For the differences between `newtype` and `data`, see Newtype.

## proc

proc (arrow abstraction) is a kind of lambda, except that it constructs an arrow instead of a function.

## qualified

Used to import a module, but not introduce a name into scope. For example, Data.Map exports lookup, which would clash with the Prelude version of lookup, to fix this:

```hs
import qualified Data.Map

f x = lookup x -- use the Prelude version
g x = Data.Map.lookup x -- use the Data.Map version

-- Of course, Data.Map is a bit of a mouthful,
-- so qualified also allows the use of as.

import qualified Data.Map as M

f x = lookup x -- use Prelude version
g x = M.lookup x -- use Data.Map version
```

## rec

The `rec` keyword can be used when the `-XDoRec` flag is given; it allows recursive bindings in a do-block.

```hs
{-# LANGUAGE DoRec #-}
justOnes = do { rec { xs <- Just (1:xs) }
              ; return (map negate xs) }
```

## type

The `type` declaration is how one introduces an alias for an algebraic data type into Haskell. As an example, when writing a compiler one often creates an alias for identifiers.

This allows you to use `Identifer` wherever you had used `String` and if something is of type `Identifier` it may be used wherever a `String` is expected.

Some common `type` declarations in the Prelude include:

```hs
type FilePath = String
type String = [Char]
type Rational = Ratio Integer
type ReadS a = String -> [(a,String)]
type ShowS = String -> String
```

## Type family

Declares a type synonym family.

## type instance

Declares a type synonym family.

## where

Used to introduce
1. a module
2. a class
3. a instance
4. a GADT
5. to bind local variables

```hs
-- (1)
module Main where

-- (2)
class Num a where

-- (3)
instance Num Int  where

-- (4)
data Something a where

-- (5)
f x = y where y = x * 2
g z | z > 2 = y where y = x * 2
```


## BNF for Haskell identifiers and operators

```go js bnf
varid      := ( small {small | large | digit | ' } ) ⟨reservedid⟩
conid      := large { small | large | digit | ' }
reservedid :=
  | module
  | import
  | type  | data     | newtype
  | infix | infixl   | infixr
  | class | deriving | instance
  | default
  | where
  | do
  | case | of
  | let  | in
  | if   | then | else
  | _
  | foreign

varsym     := ( symbol⟨:⟩ {symbol} ) ⟨reservedop | dashes⟩
consym     := ( : {symbol}) ⟨reservedop⟩
reservedop :=
  | ->        function type ctor
  | =>        context
  | ::        type annotation
  | <-        slurp
  | ..        include-the-rest operator
  | =
  | \         λ
  | `|`       sum types
  | :         list cons
  | @         as-pattern
  | ~         lazy pattern match
(specials) :=
  | !         strict pattern match (BangPatterns)
  | #         MagicHash
```



## References

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
https://en.wikipedia.org/wiki/Let_expression
http://en.wikibooks.org/wiki/Haskell/Laziness
http://haskell.org/onlinereport/decls.html
http://www.haskell.org/tutorial/modules.html

https://wiki.haskell.org/Abstract_syntax_tree
https://wiki.haskell.org/Arrow_notation
https://wiki.haskell.org/Constructor
https://wiki.haskell.org/Cunning_Newtype_Deriving
https://wiki.haskell.org/Foreign_Function_Interface
https://wiki.haskell.org/Functional_dependencies
https://wiki.haskell.org/GADT
https://wiki.haskell.org/Import
https://wiki.haskell.org/Keywords
https://wiki.haskell.org/Kind
https://wiki.haskell.org/Layout
https://wiki.haskell.org/Literate_programming
https://wiki.haskell.org/Newtype
https://wiki.haskell.org/Pattern_guard
https://wiki.haskell.org/Quasiquotation
https://wiki.haskell.org/Section_of_an_infix_operator
https://wiki.haskell.org/Smart_constructors
https://wiki.haskell.org/Smart_destructors
https://wiki.haskell.org/Template_Haskell
https://wiki.haskell.org/Type
https://wiki.haskell.org/Type_families
https://wiki.haskell.org/Type_signature
https://wiki.haskell.org/Unary_operator
https://wiki.haskell.org/View_patterns
