# Haskell Topics: Expressions

* expressions
  - let-expression
  - letrec-expression
  - equations
  - pattern matching
  - if-expression
  - case-expression
  - let-expression
  - where-expression
  - do-expression
  - lambda function

* Literals
  - character literal
  - string literal
  - integer literal
  - float literal
  - list literal
  - unit literal
  - tuple literal

* Variables
  - term variable
  - type variable
  - kind variable
  - variable as irrefutable pattern
  - variable as part of a pattern

* Patterns
  - pattern matching mechanism
  - lazy pattern matching,        `f ~(_,_)`
  - strict pattern matching,      `f !(_,_)`
  - as-pattern,                   `f @ls(x:xs)`
  - top-level as-pattern binding, `@fib(x:xs) = ...`
  - ignore pattern,               `_`
  - supress warnings pattern,     `_{identifier}`
  - variable as irrefutable pattern,  `x`
  - variable as part of a pattern,    `(x:_)`
  - view pattern
  - pattern synonym

* do notation
  - desugaring do notation
  - recursive do, `rec`
  - monadic do
  - applicative do
  - qualified do

* Functions
  - term-level functions
    - named function
    - lambda function
    - sections
    - let-expression
  - type-level functions
    - type ctor (esp. unsaturated)
    - type class
    - type families
