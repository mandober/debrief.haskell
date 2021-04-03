# TemplateHaskell

https://wiki.haskell.org/Template_Haskell

**Template Haskell** is a language extension that allows you to do *type-safe compile-time meta-programming*, with Haskell acting as both meta and object language.

Intuitively, Template Haskell provides new language features that allow us to convert back and forth between *concrete syntax* (what you type when you write normal Haskell code) and *abstract syntax* trees.

The ASTs are represented using Haskell datatypes and, at compile time, they can be manipulated by Haskell code. This allows you to *reify* (convert from concrete syntax into AST) some code, transform it and *splice* it back. You can even produce completely new code to splice in, while the compiler is compiling the module.


## Refs

Bulat's tutorials:
* http://web.archive.org/web/20100703060856/http://www.haskell.org/bz/thdoc.htm
* http://web.archive.org/web/20100703060841/http://www.haskell.org/bz/th3.htm
* A very short tutorial to understand the basics in 10 Minutes.
https://web.archive.org/web/20170331032455/www.hyperedsoftware.com/blog/entries/first-stab-th.html
* GHC Template Haskell documentation
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell
* A practical Template Haskell Tutorial
https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial

- [The user manual section on Template Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell)
- [The user manual section on quasi-quotation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation)
- [The original Template Haskell paper](http://research.microsoft.com/~simonpj/papers/meta-haskell/)
- [Notes on Template Haskell version 2](https://www.haskell.org/ghc/docs/papers/th2.ps)
- [The Template Haskell API](http://hackage.haskell.org/package/template-haskell)


## Papers about Template Haskell

* Template metaprogramming for Haskell, by Tim Sheard and Simon Peyton Jones, Oct 2002
* Template Haskell: A Report From The Field, by Ian Lynagh, May 2003
* Unrolling and Simplifying Expressions with Template Haskell, by Ian Lynagh, December 2002
* Automatic skeletons in Template Haskell, by Kevin Hammond, Jost Berthold and Rita Loogen, June 2003. [pdf]
* Optimising Embedded DSLs using Template Haskell, by Sean Seefried, Manuel Chakravarty, Gabriele Keller, March 2004. [pdf]
* Typing Template Haskell: Soft Types, by Ian Lynagh, August 2004

---

## A practical Template Haskell Tutorial

https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial

This tutorial explores GHC's compile-time meta programming in Template Haskell. It motivates use cases for meta programming and explains the different Template Haskell features on simple toy programs. The aim is to give an overview of Template Haskell's functionality in an example-driven manner.

Contents
- Introduction
- Template Haskell by Example
- Template Haskell as a Code Generator
- Generic Maps
- Reification
- Template Haskell for building EDSL
- Shakespearean Templates

### Introduction

Template Haskell (TH) is the standard framework for doing type-safe, compile-time meta programming in GHC. It allows writing Haskell meta programs, which are evaluated at compile-time, and which produce Haskell programs as the results of their execution.

Template Haskell was conceived by Tim Sheard and Simon Peyton Jones by drawing on the ideas of Lisp macros, but in the typed setting of Haskell. Since then, the original implementation has evolved, most notably, in 2007 Geoffrey Mainland added support for quasi quoting, which makes the embedding of DSLs, into Haskell as the host language, much easier.

Template Haskell has two main areas of application:
- code generation at compile-time
- facilitating the embedding of DSL

As a *code generator*, it empowers users to write many, syntactically different, programs all at once by means of a single meta program. All that is needed is a uniform, *algorithmic description* to create the different result programs, and the *meta program* then precisely implements the algorithm to compute all the different result programs as its output.

This is useful, e.g. to avoid writing the same repetitive, boilerplate code. To this end, Template Haskell is used in the *aeson* library to automatically derive a data type's `ToJSON` and `FromJSON` instances for JSON serialization; in the *lens* library to mechanically create a data type's lenses.

As a *framework for creating EDSLs*, Template Haskell allows users to embed programs written in another PL inside a Haskell's program.

This enables writing parts of the program in the concrete, domain specific syntax of a particular PL. It enables reasoning and expressing domain specific problems in the PL that is best suited for the task, letting a user focus on the domain specific problem while removing any language burden induced by inconvenient syntax, unsuitable control constructs, and similar.

Programs from the embedded language are parsed and translated into corresponding (but syntactically heavier) Haskell code at compile-time by Template Haskell. This is how the shakespearean template languages from the *shakespeare* library use Template Haskell at their core. They expose succinct domain specific languages to write HTML, CSS and JS code inside Haskell-based web application.

### Template Haskell by Example

*Meta programs* are the Haskell programs that run at compile-time and which generate Template Haskell object programs as the results of their execution; they are the programs that devise or manipulate other programs by some algorithmic means. *Object programs* are the Template Haskell programs manipulated and built by the Haskell meta programs at compile-time.

This example is about generating additional `curry`-like functions that can curry functions taking tuples of any arity (curry only works on 2-tuples), through a single meta program on demand.

```hs
-- std curry for 2-tuples
curry :: ((a, b) -> c) -> a -> b -> c
curry φ a b = φ (a, b)

-- curry3 for 3-tuples
curry3 :: ((x1, x2, x3) -> x4) -> x1 -> x2 -> x3 -> x4
curry3 f x1 x2 x3 = f (x1, x2, x3)
```

The idea is to write a meta function `curryN :: Int -> Q Exp` which, given an int `n`, constructs the source code for an `n`-ary curry function.

```hs
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Language.Haskell.TH

curryN :: Int -> Q Exp
curryN n = do
    f  <- newName "f"
    xs <- replicateM n (newName "x")
    let args = map VarP (f:xs)
        ntup = TupE (map VarE xs)
    return $ LamE args (AppE (VarE f) ntup)


-- sig is left to be inferred
curry3 :: ((x1, x2, x3) -> r) -> x1 -> x2 -> x3 -> r
-- curry3
curry3 = \f x1 x2 x3 -> f (x1, x2, x3)
          ^^^^^^^^^^       ^^^^^^^^^^
             args             ntup
```

* The meta function `curryN` doesn't deal with the signature of the resulting object function, it is only interested in function definition.

* The object function, e.g. `curryN 3` will have the form of a lambda abstraction that will bind the uncurryied function, `f`, and its 3 args (in the untupled form naturally since that's the whole point), `\f x1 x2 x3 -> ...`

* The body of the produced curry3 function will apply the original function `f` to the args, but only when all 3 args are colected because `f` expects them as a 3-tuple, `curryN 3 = \f x1 x2 x3 -> f (x1, x2, x3)`

* For input int `n`, `curryN` builds a lambda abstraction (`LamE args`) with params `f` and `n` copies of names as indexed `x`'s (x1, x2,...).

* The lambda's body the builds the expression `f (x1, x2, x3)` from instructions `(VarE f) ntup`, where `ntup` are the indexed x's made into an n-tuple, i.e. 3-tuple in case of n=3.

* The names used to refer to the variables `f` and `x1` through `xn` are generated monadically by function `newName :: String -> Q Name` to always generate fresh names not used anywhere else.

* The value returned by `curryN` is a monadic computation of type `Q Exp`. When executed, this monadic computation yields an expression `Exp` representing the object program of an n-ary curry function.

* So, `curryN 3` returns a monadic computation that yields an expression representing the function `curryN 3 :: ((a, b, c) -> d) -> a -> b -> c -> d` in the abstract syntax.

* A required curry function is not produced but merely returned by calling `curryN` with the appropriate integer representing the needed arity.

* The `curryN` is a machinery that produces expressions, in this case, Haskell's expressions, that is, functions. But basically, it contains (that is TH) all the familiar elements of an interpreter that evaluates expressions, augmented with facilities to e.g. generate fresh names and similar. `VarP` converts a generated name into a proper variable (a language entity), `VarE` refers to such entities, i.e. variables in order to engage them in constructing new expressions, like `VarE f` refers to `f` variable that was made into a variable in the first place by applying `VarP` to a name generated from "f" (which is possibly a string; the produced name might have been `f_3sY4`).

* To run a meta program such as `curryN` at compile-time, we enclose it with Template Haskell's **splice operator** (`$`) by writing, e.g. `$(curryN 3)`. This evaluates the meta program `curryN 3` and puts the resulting object program, `\f x1 x2 x3 -> f (x1, x2, x3)`, in place of the splice.
