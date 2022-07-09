# GHC compiler

At the highest level, GHC can be divided into 3 distinct chunks:

1. **The compiler itself**: this is essentially a Haskell program whose job is to convert Haskell source code into executable machine code.

2. **The Boot Libraries**. GHC comes with a set of libraries, called the boot libraries, because they constitute the libraries that the compiler itself depends on. Having these libraries in the source tree means that GHC can bootstrap itself. Some of these libraries are very tightly coupled to GHC, because they implement low-level functionality such as the `Int` type in terms of primitives defined by the compiler and runtime system. Other libraries are more high-level and compiler-independent, such as the `Data.Map` library.

3. **The Runtime System (RTS)**. This is a large library of C code that handles all the tasks associated with running the compiled Haskell code, including *garbage collection*, *thread scheduling*, *profiling*, *exception handling* and so on. The RTS is linked into every compiled Haskell program. The RTS represents a significant chunk of the development effort put into GHC, and the design decisions made there are responsible for some of Haskell's key strengths, such as its efficient support for concurrency and parallelism.

These 3 divisions correspond exactly to 3 subdirectories of a GHC source tree: `compiler`, `libraries`, and `rts` respectively.

## Code Metrics

Module
* Compiler LoC: 28,275 (1992), 139,955 (2011), increase 4.9%
  - Main
  - Parser
  - Renamer
  - Type checking
  - Desugaring
  - Core transformations
  - STG transformations
  - Data-Parallel Haskell
  - Code generation
  - Native code generation
  - LLVM code generation
  - GHCi
  - Haskell abstract syntax
  - Core language
  - STG language
  - `C--` (abstract C)
  - Identifier representations
  - Type representations
  - Prelude definitions
  - Utilities
  - Profiling
* Runtime System
  - `C` and `C--` LoC: 43,865 (1992), 48,450 (2011), increase 1.10%

## The Compiler

We can divide the compiler into 3 parts:

1. **The compilation manager**, which is responsible for the compilation of multiple Haskell source files. The job of the compilation manager is to figure out in which order to compile the different files, and to decide which modules do not need to be recompiled because none of their dependencies have changed since the last time they were compiled.

2. **The Haskell compiler** (abbreviated as `Hsc` inside GHC), which handles the compilation of a single Haskell source file. As you might imagine, most of the action happens in here. The output of Hsc depends on what backend is selected: assembly, LLVM code, or bytecode.

3. **The pipeline**, which is responsible for composing together any necessary external programs with Hsc to compile a Haskell source file to object code. For example, a Haskell source file may need preprocessing with the C preprocessor before feeding to Hsc, and the output of Hsc is usually an assembly file that must be fed into the assembler to create an object file.

The compiler is not simply an executable that performs these functions; it is itself a library with a large API that can be used to build other tools that work with Haskell source code, such as IDEs and analysis tools.

## Compiling Haskell Code

As with most compilers, compiling a Haskell source file proceeds in a sequence of phases, with the output of each phase becoming the input of the subsequent phase.

```
M.hs -> Parse -> Rename -> Typecheck -> Desugar -> Simplify ->
     -> CoreTidy -> CorePrep -> STG_conversion -> Code_generation ->
     -> Machine_code_generation




                    HsSyn Name                CoreExpr
                        |                         |
         HsSyn RdrName  |          HsSyn Id       |
              |         |             |           |
M.hs -> Parse -> Rename -> Typecheck -> Desugarer -> Simplifier -> …


                    CoreExpr
                (with tidy names)
                         |        CoreExpr
          CoreExpr       |   (with A-normal names)    STG             Cmm (C--)
             |           |            |                |                  |
… Simplifier -> CoreTidy -> CorePrep -> Convert to STG -> Code generation -> …
                         |
                Convert to IfaceSyn
              `IfaceSyn` |
                    Serialise
                         |
              M.hi (interface file)


                     |-> Pretty-print C code   -> M.hc (C code)
… Code generation -> |-> Generate machine code -> M.s (asm code)
                     |-> Generate LLVM code    -> M.ll (LLVM code)
```


### Parsing

Parsing takes as input a Haskell source file and produces as output abstract syntax. In GHC the abstract syntax datatype `HsSyn` is parameterised by the types of the identifiers it contains, so an abstract syntax tree has type `HsSyn t` for some type of identifiers `t`. This enables us to add more information to identifiers as the program passes through the various stages of the compiler, while reusing the same type of abstract syntax trees.

The output of the parser is an abstract syntax tree in which the identifiers are simple strings, which we call `RdrName`. Hence, the abstract syntax produced by the parser has type `HsSyn RdrName`.

GHC uses the tools `Alex` and `Happy` to generate its lexical analysis and parsing code respectively, which are analogous to the tools `lex` and `yacc` for C. GHC's parser is purely functional. In fact, the API of the GHC library provides a pure function called `parser` that takes a `String` (and a few other things) and returns either the parsed abstract syntax or an error message.

### Renaming

Renaming is the process of resolving all of the identifiers in the Haskell source code into fully qualified names, at the same time identifying any out-of-scope identifiers and flagging errors appropriately.

In Haskell, it is possible for a module to re-export an identifier that it imported from another module. For example, suppose module `A` defines a function called `f`, and the module `B` imports module `A` and re-exports `f`. Now, if a module `C` imports module `B`, it can refer to `f` by the name `B.f`, even though `f` is originally defined in module `A`. This is a useful form of namespace manipulation; it means that a library can use whatever module structure it likes internally, but expose a nice clean API via a few interface modules that re-export identifiers from the internal modules.

The compiler however has to resolve all this, so that it knows what each name in the source code corresponds to. We make a clean distinction between the entities, the "things themselves" (in our example, `A.f`), and the names by which the entities can be referred to (e.g. `B.f`). At any given point in the source code, there are a set of entities in scope, and each may be known by one or more different names. The job of the renamer is to replace each of the names in the compiler's internal representation of the code by a reference to a particular entity.

Sometimes a name can refer to several different entities; by itself that is not an error, but if the name is actually used, then the renamer will flag an ambiguity error and reject the program.

Renaming takes Haskell abstract syntax (`HsSyn RdrName`) as input, and also produces abstract syntax as output (`HsSyn Name`), where `Name` is a reference to a particular entity.

Resolving names is the main job of the renamer, but it performs other tasks too:
- collecting the equations of a function together, and flagging an error if they have differing numbers of arguments
- rearranging infix expressions according to the fixity of the operators
- spotting duplicate declarations
- generating warnings for unused identifiers
- and more

### Type Checking

Type checking is the process of checking that the Haskell program is correctly typed. If the program passes the type checker, then it is guaranteed to not crash at runtime. The term "crash" here has a formal definition that includes hard crashes like "segmentation fault", but not things like pattern-matching failure; the non-crash guarantee can be subverted by using certain unsafe language features, such as the FFI.

The input to the type checker is `HsSyn Name` (Haskell source with qualified names), and the output is `HsSyn Id`. An `Id` is a `Name` with some extra info, notably a type. In fact, Haskell syntax produced by the type checker is fully decorated with type information: every identifier has its type attached, and there is enough information to reconstruct the type of any subexpression (which might be useful for an IDE, for example).

In practice, type checking and renaming may be interleaved, because the *Template Haskell* feature generates code at runtime that itself needs to be renamed and type checked.

### Desugaring, and the Core language

Haskell is a rather large language, containing many different syntactic forms. It is intended to be easy for humans to read and write; there is a wide range of syntactic constructs which gives programmers plenty of flexibility in choosing the most appropriate construct for the situation at hand.

However, this flexibility means that there are often several ways to write the same code; for example, an if-expression is identical in meaning to a case-expression with "true" and "false" branches; also, the list-comprehension notation can be translated into calls to `map`, `filter`, and `concat`.

In fact, the definition of the Haskell language defines all these constructs by their translation into simpler constructs; the constructs that can be translated away like this are called "syntactic sugar". It is much simpler for the compiler if all the syntactic sugar is removed, because the subsequent optimisation passes that need to work with the Haskell program have a smaller language to deal with. The process of desugaring therefore removes all the syntactic sugar, translating the full Haskell syntax into a much smaller language that we call **Core**.

### Optimisation

Now that the program is in Core, the process of optimisation begins. One of GHC's great strengths is in optimising away layers of abstraction, and all of this work happens at the Core level. Core is a tiny functional language, but it is a tremendously flexible medium for expressing optimisations, ranging from the very high-level, such as *strictness analysis*, to the very low-level, such as *strength reduction*.

Each of the optimisation passes takes `Core` and produces `Core`. The main pass here is called the `Simplifier`, whose job it is to perform a large collection of correctness-preserving transformations, with the goal of producing a more efficient program. Some of these transformations are simple and obvious, such as *dead-code elimination* or a *case-expression reducuction* when the scrutinised value is known, and some are more involved, such as *function inlining* and *applying rewrite rules*.

**There are 6 optimisation passes** and the simplifier is normally run between them. Which passes are actually run and in which order depends on the optimisation level selected by the user.

### Code Generation

Once the Core program has been optimised, the process of code generation begins. After a couple of administrative passes, the code takes one of two routes: either it is turned into *byte code* for execution by the interactive interpreter, or it is passed to the code generator for eventual translation to *machine code*.

The code generator first converts the Core into a language called `STG`, which is essentially just Core annotated with more information required by the code generator. Then, STG is translated to `Cmm`, a low-level imperative language with an explicit stack.

From here, the code takes one of 3 routes:
* **Native code generation**: GHC contains simple native code generators for a few processor architectures. This route is fast, and generates reasonable code in most cases.
* **LLVM code generation**: The `Cmm` is converted to LLVM code and passed to the LLVM compiler. This route can produce significantly better code in some cases, although it takes longer than the native code generator.
* **C code generation**: GHC can produce ordinary C code. This route produces significantly slower code than the other two routes, but can be useful for porting GHC to new platforms.


## The syntax of Core

Expressions

```
t,e,u := 𝔁                          Value variables
       | α                          Type variables
       | τ                          Monomorphic types
       | σ                          Polymorphic types
       | 𝘒                          Data constructors
       | 𝘬                          Literals
       | λ𝔁:σ.e                     Value abstraction
       | e u                        Value application
       | Λα:η.e                     Type abstraction
       | e φ                        Type application
       | 𝙡𝙚𝙩 𝔁 : τ = e 𝙞𝙣 u          Local (let) binding
       | 𝙘𝙖𝙨𝙚 e 𝙤𝙛 p → u             Case expression
       | e|>γ                       Cast
       | ⌊γ⌋                         Coercion
    p := 𝘒 c:η 𝔁:τ                  Patterns
```


A typical work-flow for compilers of statically-typed languages is to type check the program, then transform it into some untyped intermediate language, and then perform optimizations. But GHC is different: it has a statically-typed intermediate language. As it turns out, this design choice has had a pervasive effect on the design and development of GHC.

GHC's intermediate language is called *Core* (in term of the implementation) or *System FC* (in term of underlying theory).

The exact details are not important here, but these points are the key ones:

* Haskell is a very large source language. The data type representing its syntax tree has literally hundreds of constructors. In contrast Core is a tiny, principled, lambda calculus. It has extremely few syntactic forms, yet we can translate all of Haskell into Core.

* Haskell is an implicitly-typed source language. A program may have few or no type annotations; instead it is up to the type inference algorithm to figure out the type of every binder and subexpressions. This type inference algorithm is complex, and occasionally somewhat ad hoc, reflecting the design compromises that every real programming language embodies. In contrast, Core is an explicitly-typed language. Every binder has an explicit type, and terms include explicit type abstractions and applications. Core enjoys a very simple, fast type checking algorithm that checks that the program is type correct. The algorithm is entirely straightforward; there are no ad hoc compromises.

All of GHC's analysis and optimisation passes work on Core. This is very practical since Core is a tiny language, so an optimisation has only a few cases to deal with. Although Core is small, it is extremely expressive, System F was, after all, originally developed as a foundational calculus for typed computation. When new language features are added to the source language (and that happens all the time) the changes are usually restricted to the front end; Core stays unchanged, and hence, so does most of the compiler.

But why is Core typed? After all, if the type inference engine accepts the source program, that program is presumably well-typed, and each optimisation pass presumably maintains that type-correctness. Core may enjoy a fast type checking algorithm, but why would you ever want to run it? Moreover, making Core typed, carries significant costs, because every transformation or optimisation pass must produce a well-typed program, and generating all those type annotations is often non-trivial.

Nevertheless, it has been a huge advantage to have an explicitly-typed intermediate language, for several reasons:

* Running the Core type checker (internally called `Lint`) is a very powerful consistency check on the compiler itself. Imagine that you write an "optimisation" that accidentally generates code that treats an integer value as a function, and tries to call it. The chances are that the program will segmentation fault, or fail at runtime in a bizarre way. Tracing a seg-fault back to the particular optimisation pass that broke the program is a long road. Now imagine instead that we run `Lint` after every optimisation pass (and we do, if you use the flag `-dcore-lint`): it will report a precisely located error immediately after the offending optimisation. What a blessing. Of course, *type soundness* is not the same as *type correctness*: Lint will not signal an error if you "optimise" `(x ∗ 1)` to `1` instead of to `x`. But if the program passes Lint, it will guarantee to run without seg-faults; moreover, practice has showed that it's surprisingly hard to accidentally write optimisations that are type-correct but semantically incorrect.

* The type inference algorithm for Haskell is very large and very complex and the type-checker module is by far the largest single component of GHC. "Large and complex" often means error-prone, but Lint serves as an 100% independent check on the type inference engine; if the type inference engine accepts a program that is not, in fact, type-correct, Lint will reject it. So Lint serves as a powerful auditor of the type inference engine.

* The existence of Core has also proved to be a tremendous sanity check on the design of the source language. Our users constantly suggest new features that they would like in the language. Sometimes these features are manifestly "syntactic sugar", convenient new syntax for something you can do already. But sometimes they are deeper, and it can be hard to tell how far-reaching the feature is. Core gives us a precise way to evaluate such features. If the feature can readily be translated into Core, that reassures us that nothing fundamentally new is going on: the new feature is syntactic-sugar-like. On the other hand, if it would require an extension to Core, then we think much, much more carefully.

In practice Core has been incredibly stable: over a 20-year time period we have added exactly one new major feature to Core (namely coercions and their associated casts). Over the same period, the source language has evolved enormously. We attribute this stability not to our own brilliance, but rather to the fact that Core is based directly on one of the foundational mathematical theory: bravo Girard!.
