# Desugaring

* compile but tell GHC to output the result after the desugaring phase:
`ghc -c Foo.hs -ddump-ds`


Haskell's core language is very small and most Haskell code desugars to either:
- lambdas and function application
- algebraic data types
- case expressions
- recursive let bindings
- type classes and specialization
- foreign function calls


Examples of higher-level features desugared to the core-level primitives:

## if-else

`if` is equivalent to a `case` statement:

```hs
if b then e1 else e2
-- is equivalent to
case b of
  True  -> e1
  False -> e2

-- this works because Bool is defined within the std:
data Bool = False | True
```

## Functions

* Polyadic lambdas are equivalent to nested lambdas of single arguments:
* Polyadic functions are curried lambdas functions
* Infix ops (binary functions) desugar to their prefix (parenthesized) form
* Infix (ticked) functions desugar to their prefix (tick-free) form
* GHC distinguishes operators from functions by reserving a special set of symbols (punctuation symbols) for the operators.

```hs
-- polyadic lambdas
\x y z -> e
-- equivalent to curried lambda
\x -> \y -> \z -> e


-- polyadic function
f x y z = e
-- is equivalent to
f = \x y z -> e
-- which in turn desugars to
f = \x -> \y -> \z -> e


-- infix function form using backticks
x `op` y
-- desugars to
op x y


-- infix operators
x + y
-- desugar to prefix form
(+) x y


-- sectioning, LHS
(1+)
-- desugars to:
\x -> 1 + x

-- sectioning, RHS
(+1)
-- desugars to:
\x -> x + 1

-- also works with infixed functions:
(`f` 1)
-- desugars to:
\x -> x `f` 1
-- then desugars to:
\x -> f x 1
```


## Operator parameters

* the parentheses trick for operators also works in other contexts.
* bind params using symbolic names by surrounding them with parentheses.

```hs
let f (%) x y = x % y
in  f (*) 4 2

-- desugars to
(\(%) x y -> x % y) (*) 4 2

-- which reduces to
4 * 2
```

* even type params can have symbolic names
