# Type system in Haskell

## Type system entities
- terms
  - expression
  - value
  - thunk
  - redex
  - normal form
  - special form
  - weak head normal form
  - data ctor
    - data ctor fields
    - data ctor accessor function
    - variants (sum type)
- types
  - type
  - type ctor
  - data ctor
  - product type
  - sum type
  - promoted data ctor
  - kind
  - Constraint
  - type class
  - type equality


## Values, types and kinds

In Haskell, values are classified into types, and types are classified into kinds. Types and kinds live at the term-level.

Before, it could have been said that kinds live at the kind level, but around GHC v9.0, the axiom `TypeInType` was introduced which made types and kinds practically the same. That is, types are now said to be classified by other types, which in such role are called kinds. Internally, however, there is no difference between types and kinds.

This equality of types and kinds is a step toward introducing dependent types into Haskell, which blur the difference between types and values. For example, in a dependently-typed language like Agda, values can be used at the type level, and types can be returned from (term-level) functions. However, even in GHC v.9.8, we still talk about values, types and kinds, but we consider kinds as another flavour of types. We still use the commands `:t` and `:k` in GHCi to check the type and kind of a term-level or type-level expression; in fact, `:k!` is used to reduce the kind of a type-level expression, which often occurs in type-level computations.


## Term-level

### Expression, values, reduction, evaluation

value-level, term-level
type-level
expression
thunk
normal order
applicative order 
laziness
strictness
value
term
data
reduction
evaluation
partial evaluation
redex
weak-head normal form (whnf)
normal form (nf)
forcing
pattern matching sites
  pattern matching (lhs of equation)
  case expression
  lambda args
  slurp results
seq
deepseq
shape of data


Expressions, values, terms, data, and such are names we use to reference things that live at the term-level. It is probably best to pick one name and always stick with it when referring to such entities: so *terms* live at the *term-level*.

*Expression* is perhaps the broadest category of such entities, although the name "expression" is also used to name things at the type level, but, by default, it refers to the term-level.

Expressions and values are different entities.

A **term-level expression** usually occurs as a **thunk**, which is a reducable expression, also called a **redex**. Such an expression, when completely evaluated, may reduce down to a **value**. When it does, the process is called **evaluation**. In other words, the process of evaluation evaluates an expression (redex) down to a value. However, if the redex cannot be evaluated to a value, because the process stopped for some reason, it is left as an expression still, and in this case, we called the process **reduction**. So evaluation and reduction are basically the same processes, except the former produces **values**, while the latter produces **partially evaluated expressions**. It can be said that a value is an expression that cannot be evaluated any further, that has reached its **normal form**.

In some circumstances, an expression cannot be evaluated to a value because of error. Actually, the expression is then also reduced down to a value, although it is a special sort of value called **bottom**. Bottom signals **divergent computation**. The value bottom is a member of each type in Haskell.

exp -> thunk
-> evaluation -> partial evaluation -> whnf -> nf (value)
-> reduction -> partial reduction -> partially evaled exp | bottom


**Term-level expressions** come in several forms:
- unevaluted (untouched) expressions, usually in the form of *thunks*.
- *weak-head normal form* (whnf): expressions that are evaluated just enough to reveal their data constructor (usually happens in pattern matching).
- *normal form*: expressions that are evaluated all the way down to a *value*.

An expression is usually sitting behind a thunk, so an unevaluted expression could be just called a thunk. When a function is applied to an argument, that argument, as a thunk, is passed unevaluted into the function because Haskell evaluates things using *normal order strategy*. It is constrasted by the applicative order, where the argument is evaluted before being passed into a function.

Inside the function, the argument (as a thunk) usually goes through the pattern matching, which can happen on the left-hand side of a function equation, or on the right-hand side in the `case` expression. Anyway, the argument gets evaluated just enough so as to reveal its data constructor (all values have data ctors, even the lifted primitives like `Int`, whose ctor is `I#`). The argument evaluated just barely enough to reveal its data ctor is said to be in *weak-head normal form*. What's described is what happens most of the time, but it can be influenced with various mechanisms that increase or decrease the strictness (laziness) level.

When an argument is completely evaluated it is in the *normal form*. This can happen naturally, through computation (e.g. because it needs to be printed), or through *forcing*, which also has a few levels in terms of how much evaluation is performed, e.g. `seq` will force an arg to whnf, `deepseq` will evaluate an argument as much as possible, including its substructures (if it's a compound data structure).

Reduction and evaluation are practically synonyms, except that their end result could be different: when an expression reduces all the way down to a value, we talk about *evaluation*. However, if the expression can be reduced but not down to something that could reasonably be called a value, we tal kabout *reduction*.

Evalution is done is steps, where each step peels off a layer, reducing the expression a little bit. Usually, the initial step peels off a layer and reveals the **data ctor** used to construct that expression.




## Simply Easy! An Implementation of a Dependently Typed Lambda Calculus
Andres Loh, Conor McBride, Wouter Swierstra, 2007

Haskell's type system can be divided into 3 levels:
- expressions
- types
- kinds

Programmers write expressions, and the type checker ensures they are well-typed. The type language itself is extremely rich. For instance, data types, such as lists, can abstract over the type of their elements. The type language is so complex that the types themselves have types, called kinds. The kind system itself is relatively simple: all types inhabited by expressions have kind `Type`; type constructors, such as lists, have a 'function kind', in the same way as lambda expressions have a 'function type'.

With its 3 levels, Haskell is a rich version of the typed lambda calculus called `Fω`, which also forms the basis of the core language used in GHC. Compared to pure `Fω`, full Haskell is augmented with lots of additional features, most notably the facility to define your own data types and a cunning type inference algorithm.

In this section, we consider the *simply-typed lambda calculus*, or `λ→` for short. It has a much simpler structure than `Fω` as there is no polymorphism or kind system. Every term is explicitly typed and no type inference is performed. In a sense, λ→ is the smallest imaginable statically typed functional language.

### Abstract syntax

```hs
τ := α                -- base type
   | τ → τʹ           -- function type

e := e : τ            -- annotated term
   | x                -- variable
   | e₁ e₂            -- application
   | λx → e           -- lambda abstraction

v := n                -- neutral term
   | λx → v           -- lambda abstraction

n := x                -- variable
   | n v              -- application
```


The type language of λ→ consists of just 2 constructs:

```hs
τ := α                base type
   | τ → τʹ           function type
```

There is a set of **types**
- base types `α`
- compound types, `τ → τʹ`, correspond to functions from `τ` to `τʹ`

```hs
e := e : τ            annotated term
   | x                variable
   | e₁ e₂            application
   | λx → e           lambda abstraction (body is an exp)
```

There are 4 kinds of **terms**:
- terms with an explicit type annotation
- variables
- applications
- lambda abstractions

Terms can be evaluated to **values** (domain values, i.e. the end values):

```hs
v := n                neutral term
   | λx → v           lambda abstraction (body is a value)

n := x                variable
   | n v              application
```

A value is either a
- neutral term, `n` (var applied to a possibly empty sequence of values)
  - variable, `x`
  - application, `n v`
- lambda abstraction, `λx → v`, whose body is a value
