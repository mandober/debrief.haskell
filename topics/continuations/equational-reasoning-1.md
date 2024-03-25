# Equational reasoning

## TL/DR

*Equational reasoning* is the name for a process used to reason about a piece of code, or an entire program even.

Equational reasoning is usually associated with pure FPLs like Haskell, although, it can also be (carefully) employed with impure FPLs, such as SML, OCaml, Scheme.

The essential property of a language that makes equational reasoning possible is *referential transparency*, which implies the absence of (observable) mutability and side-effects. Once declared|defined, an identifier binds its argument|contents forever, you cannot rebind it and shit; it's like SSA (static single assignment form)



*Equational reasoning* names the specific|particular process|approach people use when trying to manually (mentally) evaluate a piece of code.


process to a collection of things that we rely upon when 




evaluate, a 

Reasoning about a program, usually by evaluating it mentally, 

Evaluating a program by replacing equals for equals.

## Expression

A program in FP is mostly built out of expressions. These pure expressions, free of side-effects, can be thought of as being the same as definitions in a logical system. After all, programming languages are formal systems, and also symbolic systems, and some are even logical.

consisting of a definiendum and definiens



## Evaluation

In Haskell, programs are executed by evaluating expressions that make up the program. We can even do this easily mentally thanks to *equational reasoning*.


because all expressions are pure and **referentially transparent**, lacking the side effects to watch out for, and having no particular order in which the evaluation should be performed. The only way to introduce an ordering is through data dependency, that is, to make one expression depend on another.

## Prelude

In pure functional languages like Haskell, programs are built out of expressions, which take different forms but always retain the fundamental property of equations: the part on the right-hand side (of an equals sign) introduces a new name (into the current environment), while the part on the left-hand side specifies the meaning of the name by a definition.

definiendum = definiens

**definiendum**: a word, phrase, or symbol which is the subject of a definition (especially in a dictionary entry), or which is introduced into a logical system by being defined.

**definiens**: a word, phrase, or symbolic expression used to define something (especially in a dictionary entry), or introducing a word or symbol into a logical system by *providing a statement of its meaning*.



by associating it with an expression that is its definition.


supplying its definition.

it defines the name by providing a definition.


defines it, and the whole thing is called a definition or an equation.

The execution of a functional program means repeatedly evaluating the expressions that make up the program, performing a lot of substitutions along the way, until no further reductions are possible. Then the final value is the overall value of the program.

An *expression* is a syntactic form which when evaluated produces a value. A very different syntactic form is a *statement* which when executed produces an effect rather than a value. Almost all PLs have both forms, but the statements 



In FP, almost everything in a program is an expression. 


An opposing example is found in imperative programs, which, naturally, also have expressions, but they have statements as well.

another syntactic group called 

A simple expression has no proper subexpressions.


the execution of a program proceeds by 
programs are executed by 
repeatedly evaluating the expressions that make up the program. 

We can even do this manually (mentally), which is called *equational reasoning*, because the expressions are pure and referentially transparent, so there are no side effects to watch out for, nor is there a particular order in which the exp should be evaluated. The only factor that can dictate the order of evaluations is the data dependency - when one exp depends on the value of another.
