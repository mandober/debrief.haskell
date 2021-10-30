# Template Haskell tutorial
https://markkarpov.com/tutorial/th.html

## Motivation

Some common use cases of TH:

* Automatic deriving of type class instances. Although the same can often be addressed by generics, they are known to make compilation times longer, so TH is still the preferred method in libraries like `aeson` and `lens`.

* Creation of TH DSLs that are integrated into systems built in Haskell. Examples of such DLSs are the language for model declaration used in `persistent`, and various other mini-languages used in `yesod` web framework.

* Compile-time construction of values of *refined types* that turns invalid inputs into compilation failures.

* Compile-time loading and processing of data from external files, which is very useful sometimes. Even though this involves running IO during compilation, it's a relatively innocent use case of that dangerous feature.

Reasons not to use TH:
- TH helpers are often viewed as black boxes that do "magic". It is not clear at all what a thing of the type `Q [Dec]` does, it might do anything; any code that generates declarations has the same `Q [Dec]` type, no matter what sort of declarations it generates. Documentation becomes the main source of information about semantics of TH code.
- TH imposes restrictions on where the user should define TH functions themselves and sometimes also how to order definitions in files where TH functions are used.

## The Q monad

Generation of code requires certain features to be available to us:
- The ability to generate new unique names that cannot be captured.
- The ability to retrieve information about a thing by its name. Usually we want to know about functions and types, but there are also ways to learn about a module, get collection of instances of a particular type class, etc.
- The ability to put and get some custom state that is then shared by all TH code in the same module.
- The ability to run IO during compilation, so we can e.g. read something from a file.

These features are usually achieved through monads in Haskell, and so it should not come as a surprise that there is a special monad called `Q` (Q for *quotation*) that hosts all functions provided by TH.

## Splicing

The only purpose of having a value of the type `Q a` is to use `a` in a program somehow. That `a` can be anything in intermediate monadic expressions, but when we're about to insert the generated code into a Haskell source file, there are only 5 options:

* Declaration `Dec`, which includes the top-level things like function and data type definitions. In fact, we'd like to be able to generate several declarations at a time, so the type that is actually used (and expected by the interpolating machinery) is `[Dec]`.

* Expression `Exp`, such as `x + 1` or `\x -> x + 1`. It is probably the most common thing to generate.

* Typed expression `TExp`, which is identical to expression `Exp`, but has a phantom type tag, `TExp a` corresponding to the type of the expression inside. For example, `TExp Int` means that the expression evaluates to an `Int`.

* Type `Type` such as `Int` or `Maybe Int` or `Maybe`. The type doesn't have to be saturated (i.e. may have any kind), so it may be pretty much anything one can encounter on the type level.

* Pattern `Pat` used for pattern-matching.

See the docs for `Dec`, `Exp`, `TExp`, `Type`, and `Pat` and note the naming convention: the data constructors are suffixed with letters that hint about the data type they belong to: `Dec` constructors end with a `D`, `Exp` constructors end with an `E`, Type constructors end with a `T`, and `Pat` constructors end with a `P`. This makes it easy to distinguish e.g. an expression variable `VarE` from a pattern variable `VarP`.

Using the data types, we can construct an expression:

```hs
myFunc :: Q Exp
myFunc = do
  x <- newName "x" -- generate a unique var name
  return $ LamE    -- lambda expression
    [VarP x]       -- pattern matching on 'x'
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
    -- here we have an infix expression:
    -- we apply (+) to 'x' and integer literal 1
```

The TemplateHaskell language extension enables the special syntax `$(exp)` where `exp` is an arbitrary expression producing `Q [Dec]`, `Q Exp`, `Q Type`, or `Q Pat`. This allows interpolation of the generated code into normal Haskell source code.

For example, I can now use `myFunc` like this:

```hs
λ> :set -XTemplateHaskell

-- The parentheses are not necessary if myFunc doesn't take args.
-- If it does, it looks like: $(myFunc arg) 3
-- In other words, parentheses are only needed around expressions.
λ> $(myFunc) 3 -- 4
λ> $myFunc 3   -- 4

λ> let f = (* 2) . $myFunc
λ> f 10 -- 22
```

This is called **splicing**.

The expression after the dollar sign is called a *splice*. A splice can occur in place of an expression, a pattern, a type, or as a top-level declaration.

It is worth noting that declarations may be spliced without the preceding `$` because they live on the top-level and there is no syntactical ambiguity.

`makeLens` from the lens package is a common example:

```hs
makeLens ''MyRecord
-- the same as:
$(makeLens ''MyRecord)
```

Note that the `$` symbol now has an additional meaning and so ambiguity is possible in some cases. When `$` is used in splices, there must be no space between `$` and the identifier or opening parenthesis that follows after it.

To use `$` as the inxif application operator, `f $ x`, be sure to add at least one space between the operator and the following code.

## Limitations of TH

Using TH currently has some limitations:

* *Staging restriction*, which means that inside a splice one can only use functions that are already compiled, i.e. defined in other modules, not in the same module that contains the splice. This is a pretty nasty limitation that makes developers have a separate module for TH code, typically called `TH`.

* TH often makes you order your definitions in a particular way.

Top-level declaration splices break up a source file into declaration groups.

A declaration group is the group of declarations created by a top-level declaration splice, plus those following it, down to but not including the next top-level declaration splice.

> Only top-level splices delimit declaration groups, not expression splices.

The first declaration group in a module includes all top-level definitions down to but not including the first top-level declaration splice.

Each declaration group is mutually recursive only within the group.

Declaration groups can refer to definitions within previous groups, but not later ones.

## Quotation

As we have seen, the Haskell AST that TH can build and manipulate is not small and not easy to work with at all. Unfortunately, it is also possible to produce an AST of a correct shape that does not represent a Haskell program that compiles. In other words, manual construction of AST is tedious and error-prone.

Luckily, there is a way to get AST of arbitrary Haskell code by using quotation. There are 4 types of quotations that are enabled by the `TemplateHaskell` language extension:

Thing produced  | Quotation syntax  | Type
----------------|-------------------|--------------
Declaration     | [d| … |]          | Q [Dec]
Expression      | [e| … |]          | Q Exp
Expression      | [|  … |]          | Q Exp
Typed expr      | [|| … ||]         | Q (TExp a)
Type            | [t| … |]          | Q Type
Pattern         | [p| … |]          | Q Pat

Since most of the time we work with expressions, the more lightweight quote syntax `[| … |]` is equivalent to `[e| … |]`.

We need several different quoters because the same code may mean different things is different contexts, for example:

```hs
-- a pattern
λ> runQ [p| Just x |]
ConP GHC.Base.Just [VarP x_0]

-- an expression
λ> runQ [e| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)

-- an expression again
runQ [| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)
```
