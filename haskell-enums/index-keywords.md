# Index of keywords

https://wiki.haskell.org/Keywords

Identifiers and Operators

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

* Haskell keywords
  * Reserved keyords
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

  * Reserved keywords in some contexts
    - `as`          in the import statement
    - `qualified`   in the import statement
    - `hiding`      in the import statement
    - `forall`      at the type level
    - `foreign`     with FFI extension

  * Reserved symbols (compiler builtins)
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

  * Type names:
    - Bool
    - Ordering
    - Char
    - String
    - Int
    - Integer
    - Float
    - Double
    - Fractional


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


## Bang
Whenever a data ctor is applied, each arg to the ctor is evaluated iff the corresponding type in the algebraic datatype declaration has a *strictness flag*, denoted by a bang. For example:

```hs
data STList a
    = STNil
    | STCons a !(STList a)
--             ^^^^^^^^^^^
-- the second arg to STCons will be evaluated before STCons is applied


-- Illustrating the difference between
--   STRICT VS LAZY CTOR APPLICATION

strictList = STCons 1 undefined
lazyList   = (:)    1 undefined

strictHead (STCons h _) = h -- when applied to strictList, evals to: undefined
lazyHead   (h : _)      = h -- when applied to lazyList,   evals to: 1
```


### Bang Patterns

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BangPatterns


The bang is also used in the *BangPatterns* GHC extension to enforce strictness of a pattern in pattern-matching:

```hs
-- first evals <expr> to WHNF then matches the results:
f !pat = expr


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
