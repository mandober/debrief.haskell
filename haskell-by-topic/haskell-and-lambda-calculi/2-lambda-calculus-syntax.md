# Haskell by Topic :: Haskell and Lambda Calculi

## Lambda Calculus syntax

### Abstract and concrete syntax

The first step in implementing a programming language is to define its syntax. The **syntax** of a language is defined by a *grammar*, which is a set of rules that describes how to form *valid expressions* in the language.

There are two kinds of syntax:
- abstract syntax
- concrete syntax

The **abstract syntax** of a programming language is a data structure that represents the structure of expressions in the language, commonly in the form of an abstract syntax tree (AST) since expressions form a tree.

The **concrete syntax** is the *textual representation* of language expressions. It is a string of characters that represent a program.

The first thing then to do is convert the textual representation of a language into an AST using a parser (which can optionally contain a lexer and tokenizer).

## The grammar of untyped lambda calculus

Untyped lambda calculus may be considered a rudimentary programming language that only has 3 terms: variables, abstractions and applications.

The grammar of a language is usually defined using the BNF notation, and the grammar of untyped lambda calculus has only these 3 terms:

```hs
exp := var          -- variables
     | abs var exp  -- abstraction
     | app exp exp  -- application
```

The `exp` is a metavariable that ranges over all LC terms.

It means that a term (exp) of the language is either
- `var`: a variable like `x`, `y`, etc.
- `abs var exp`: abstraction `λx.M`, where M is any `exp`
- `app exp exp`: application `M N`, where `M` and `N` are any `exp`

Any of those 3 terms represent a valid program, as do any recursive combinations thereof.

A *lambda term* is one of these 3 forms.

A *lambda expression* is one of the 3 lambda terms, as well as any legal recursive combination thereof. In general, any arbitrary expression composed using the valid combination of lambda terms is called a lambda expression. However, no matter how complicated that expression may be, it will always be one of the 3 terms, so there is no difference between lambda terms and lambda expressions, and they are often used interchangibly.

Even though the phrase "recursive combination of lambda terms" is used to describe a valid lambda expression, it is actually hard to come up with an invalid expression by freely combining the 3 terms. Even a series of variables like `xyz` is a lambda expression, although it might be undefined (i.e. have no meaning) if these 3 variables are free and comprise the entire expression. This is primarily because the application is represented as juxtaposition.

The BNF grammar usign a concrete syntax is:

```hs
E := x
   | λ x . E
   | E E
```

where `x` is variable, and `E` is a metavariable representing any term. The literal symbol `λ` is used to denote the binder of the abstraction, which binds a variable; it is separated from the lambda's body, that contains an expression `E`, by a literal dot character.

The terms should be parenthesized to remove any ambiguity. Some parentheses can be removed by introducing associativity and precedence:
- application has higher precedence then abstraction, although parens should 
  take care of this; λy. x y = λy.(x y) ≠ (λy.x)y
- application is left-associative: M N P Q = ((M N) P) Q
- abstraction is right-associative: it extends as far to the right as possible
  λx.λy.λz.xzy = λx.(λy.(λz.xzy)) = λx.(λy.(λz.(xz)y))
  λx.λy.λz.x(λw.w)zy = λx.(λy.(λz.((x(λw.w))z)y))
- lambda binders in an abstraction may be compacted, λx.λy.λz.E = λxyz.E
