# Haskell by Topic :: Haskell and Lambda Calculi

## Implementing a language

From the book: `Write You A Haskell - Building a modern functional compiler from first principles` by Stephen Diehl, 2015

Troughout this discussion we stress the importance of semantics and the construction of core calculi. The *frontend language syntax* will be based on the ML-family of languages. Choice of *lexical syntax* is arbitrary. If there is one central theme, it is that the design of the *core calculus* should drive development, not the frontend language.

## Concepts

We set out to build a statically typed functional programming language with a native code generation backend.


From source files to binaries
- Source files
- Lexing
- Parsing
- Desugaring
- Typing phase
  - type-inference
  - type-checking
  - type-erasure
- Transformation phase
  - Renaming
  - Macro expansion (splicing)
  - Optimizations
- Compilation
  - Interpretation
  - Code generatation
- Binary files (with machine code)


Frontend lang → Core lang → IR → Machine code



## Functional Languages

In mathematics, a *function* is defined as a correspondence (rule, association) that relates each element of a domain set to exactly one element of a codomain set. Math functions are pure: given the same arguments, a function will always return the same vlaue. If a function `f(x) = y`, then the function evaluated at `x` will always have the value `y`. Central to the notion of mathematics is the notion of *equational reasoning*, where if `y = f(x)`, then an expression like `g(f(x), f(x))` is always equal to `g(y, y)`. In other words, the values computed by functions can always be substituted freely at all occurrences.

The central idea of pure functional programming is to structure the programs in such a way that we can reason about them as a *system of equations* just like we can in mathematics. The evaluation of a *pure function* is one in which side effects are prohibited; a function may only return a result without altering the world in any observable way. The implementation may perform effects, but central to this definition is the *unobservability of effects*. A function is said to be *referentially transparent* if replacing a function with its computed value yields the same *observable behavior*. By contrast impure functions are ones which allow unrestricted and observable side effects. The invocation of an *impure function* always allows for the possibility of performing any *side effects* (launching rockets) before yielding a value.

Thus, the behavior of an impure function is intrinsically tied to its execution order - the evaluation of an impure function must be known, stated up front in a once-and-for-all manner, and completely deterministic due to all the hidden dependencies. On the other hand, the evaluation of pure functions need not be known or even described anywhere. The behavior of a pure function is utterly divorced from the location where it is defined or the time when it is evaluated. This leaves the compiler much freedom to figure out the best evaluation strategy on our behalf.

## Static Typing

Types are a *formal language* integrated with a programming language. The goal of types is to constrain the space of possible programs to only the correct ones. Types are the most popular formal method for analyzing programs and providing program safety and correctness.

On the other hand, in dynamic languages, like JS and Python, all expressions have the same type at compile time, and all syntactically valid programs can be and will be evaluated. When a program is nonsensical, the runtime will bubble up exceptions during evaluation, eventually blowing up. The interpreters of these languages make no attempt to analyze programs for *soundness* before running them. By contrast, Haskell will do quite a bit of work to try to ensure that the program is well-defined before running it. The language used to prescribe and analyze *static semantics* of programs is that of *static types*.

The gradual trend over the last 20 years of CS goes toward more expressive type systems capable of expressing bigger and bigger variaties of correct programs and ensuring program correctness. Namely, type systems have not yet reached the point that all correct programs can be typed - in some (fringe) cases, correct programs will be rejected despite being well-formed because it is better to reject a small portion of correct programs then to allow incorrect ones. however, the type systems keep getting better, decreasing this gap between correct programs versus typable programs. Even though type systems will probably never be able to completely cover this gap, modern sophisticated type systems are incredibly helpful. Static types let us relax and offload the sanity checks that we would otherwise have to perform ourselves. They even help us reason about program behaviors and assist in interactively building programs and proving things about them.

## Functional Compilers

A compiler is a program for turning high-level representation of ideas in a human readable language into another form.

A compiler typically consists of a frontend and a backend. The *frontend* deals with converting the human representation of the code into a *canonicalized form*, while the *backend* converts the canonicalized form into another form that is suitable for evaluation.

The high level structure of functional compilers is described by the different phases the compiler goes through. A *phase* is a sequence of transformations composed to transform the input program.
- Source phase: frontend textual source language; reading program src files.
- Parsing phase: source is parsed and lexed, making an AST.
- Desugaring phase: redundant lang structures are canonicalized.
- Type-checking phase: missing types are inferred, program is type-checked, yielding an explicitly typed intermediate form - core language.
- Transformation phase: core language is further transformed and optimized.
- Compilation phase: core language is "lowered" into a form that can be more conveniently interpreted or compiled.
- Code-generation phase: platform specific code is generated, resources are linked into the executable binary.

A *pass* may transform the input program from one form into another or alter the internal state of the compiler context.

## Parsing

*Source code* is the raw sequence of text that specifies the program. *Lexing* splits the text stream into a sequence of *tokens*. Only the presence of invalid symbols is checked; programs that are invalid in other ways are still accepted. Whitespace is either ignored or represented as a unique token.

Token stream can then be scanned via dispatch on predefined patterns of tokens called *productions* to recursively build up the *syntax data type* for the *abstract syntax tree* (AST).

## Desugaring

Desugaring is the process by which the frontend AST is transformed into a simpler form of itself by reducing the number of complex structures by expressing them in terms of a fixed set of simpler constructs.

Haskell's frontend is very large and many constructs are simplified down. For example, `where` clauses are effectively syntactic sugar for let-bindings; operator sections are desugared into lambdas.

## Type Inference

Type inference is the process by which the untyped syntax is endowed with type information by a process known as *type reconstruction* or type inference. The inference process takes into account the existing user *type annotations*.

The inference procedure generates a *system of constraints* which are then solved via the process called *unification*, in order to calculate the type of every expression. In some cases, the types will be incorporated directly into the AST and the inference will transform the frontend language into an explicitly typed *core language*.

## Transformation

The core language is usually the most suitable for evaluation, but quite often different intermediate representations are more amenable to certain optimizations and make various semantic properties of the language explicit. These kind of *intermediate forms* may attach information about free variables, allocations, and usage information directly in the AST structure.

The *Spineless Tagless G-Machine* (STG) is an abstract machine used in compiling FPLs that makes many properties of lazy evaluation explicit, directly embedding that information into the AST.

## Code Generation

After translation, the program expressed in the core language can either be interpreted or translated further into another intermediate language (such as *LLVM IR* or GHC's *Cmm*) which can be compiled into native code. These intermediate languages abstract over the process of assigning values to, and moving values between, CPU registers and main memory.

From the intermediate representation the code can be compiled into a suitable *assembly language*. Any additional code that is required for execution is *linked* into the resulting module. Ultimately, the generated code will be assembled into platform specific instructions by the *native code generator*, encoded as a predefined sequence of CPU instructions as defined by the particualr processor architecture.
