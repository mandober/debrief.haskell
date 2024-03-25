# Template Haskell tutorial

https://markkarpov.com/tutorial/th.html

TH helpers are often viewed as black boxes that do "magic". It is not clear at all what a thing of the type `Q [Dec]` does, it might do anything; any code that generates declarations has the same `Q [Dec]` type, no matter what sort of declarations it generates. Documentation becomes the main source of information about semantics of TH code. TH imposes restrictions on where the user should define TH functions themselves and sometimes also how to order definitions in files where TH functions are used.

## The Q monad

Generation of code requires certain features to be available to us:
- ability to generate new unique names that cannot be captured.
- ability to retrieve information about a thing by its name. 
  Usually we want to know about functions and types, but there are also ways to learn about a module, get a set of instances of a particular type class, etc.
- ability to put and get a custom state shared by all TH code in the same module
- ability to run IO during compilation, so we can e.g. read a file

The special monad called `Q` (Q for quotation) hosts all TH features.

## Splicing

The only purpose of having a value of the type `Q a` is to use `a` in a program somehow. The `a` can be anything in intermediate monadic exps, but when we're about to insert the generated code into a source file there are only 5 options:

`[Dec]`, `Exp`, `TExp ph`, `Type` and `Pat`

1. Declaration, `Dec`, [Dec]   
  which includes top-level things like function and data type definitions. We can generate several declarations at a time, so the actual type is `[Dec]`.

2. Expression, `Exp`   
  such as `x + 1` or `\x -> x + 1`. These are the most common thing to generate.

3. Typed expression, `TExp`, `TExp ph`   
  which is identical to expression `Exp`, but has a phantom type tag; `TExp ph` corresponds to the type `ph` of the exp inside; e.g. `TExp Int` means that the exp evaluates to an `Int`.

4. Type, `Type`   
   such as `Int`, `Maybe Int`, even `Maybe`. The type doesn't have to be saturated (i.e. may have any kind), so it may be anything one can encounter on the type level.

5. Pattern, `Pat` used for pattern-matching

The naming convention: the data ctors are suffixed with letters that hint about the data type they belong to:
- `Dec`  ctors end with a `D`
- `Exp`  ctors end with a `E`
- `Pat`  ctors end with a `P`
- `Type` ctors end with a `T`
- literals end with a `L`

This makes it easy to distinguish e.g.
- LamE      lambda exp
- VarP      pattern var
- VarE      exp var
- InfixE    infix exp
- LitE      literal exp
- IntegerL  integer literal
- Q monad `Q a` inst as `Q Exp`


Using the data types, we can construct an exp:

```hs
myFunc :: Q Exp
myFunc = do
  x <- newName "x" -- generate a unique var name
  return $ LamE
    [VarP x]       -- pattern matching on 'x'
    -- apply (+) to 'x' and int literal 1
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
```


`TemplateHaskell` extension enables the syntax `$(exp)`, called a *slice*, where `exp` is an arbitrary exp producing `Q [Dec]`, `Q Exp`, `Q Type`, `Q Pat`. This allows interpolation of the generated code into normal Haskell source code.

For example, I can now use `myFunc` like this:

```hs
:set -XTemplateHaskell

-- parens not necessary if myFunc does not take args
-- if it does, it looks like: $(myFunc arg) 3
-- In other words, parentheses are only needed around exps.

$(myFunc) 3   == 4
$myFunc 3     == 4

let f = (* 2) . $myFunc
f 10          == 22
```

This is called **splicing**. The exp after the dollar sign is called a *splice*. A splice can occur in place of an exp, pattern, type, or as a top-level declaration.

Declarations may be spliced without the preceding `$` because they live on the top-level and there is no syntactical ambiguity.

The `makeLens` fn from the `lens` package is a common example:

```hs
makeLens ''MyRecord
-- same as:
$(makeLens ''MyRecord)
```

Note that the `$` symbol now has an additional meaning and so ambiguity is possible in some cases. When `$` is used in splices, there must be *no space between `$` and the identifier and parens*. To use `$` as the infix operator (f $ x) space it out.


## Limitations of TH

* *Staging restriction* means that inside a splice one can only use functions that are already compiled - defined in other modules, not in the same module that contains the splice. This is a pretty nasty limitation that makes developers have a separate module for TH code, typically called TH.

* TH often makes you order definitions in a particular way.

Top-level declaration splices break up a source file into declaration groups. A **declaration group** is the group of declarations created by a top-level declaration splice, plus those following it, down to but not including the next top-level declaration splice.

> Only top-level splices delimit declaration groups, not expression splices.

- The first declaration group in a module includes all top-level definitions down to but not including the first top-level declaration splice.
- Each declaration group is mutually recursive only within the group.
- Declaration groups can refer to definitions within previous groups, but not later groups.

An example: suppose we want to use the lens library to generate some lenses. We could have code like this:

```hs
data MyRecord = MyRecord         -- <<< first declaration group
  { _myRecordFoo :: Foo          --
  , _myRecordBar :: Bar          --
  , _myRecordBaz :: Baz          --
  }                              --
                                 --
getRecordFoo :: MyRecord -> Foo  --
getRecordFoo = view myRecordFoo  --
                                 --
makeLenses ''MyRecord            -- <<< second declaration group
-- ^ Generates lenses: 'myRecordFoo', 'myRecordBar' and 'myRecordBaz'.
```

Sadly, this code won't compile. The first declaration group includes the definitions of `MyRecord` and `getRecordFoo`, but not the generated lenses. This means that `myRecordFoo` is out of scope in `getRecordFoo`.

We could fix this by placing `getRecordFoo` after the makeLenses `''MyRecord` splice:

```hs
data MyRecord = MyRecord         -- <<< first declaration group
  { _myRecordFoo :: Foo          --
  , _myRecordBar :: Bar          --
  , _myRecordBaz :: Baz          --
  }                              --
                                 --
makeLenses ''MyRecord            -- <<< second declaration group
                                 --
getRecordFoo :: MyRecord -> Foo  -- can see 'MyRecord' from the
getRecordFoo = view myRecordFoo  -- previous group
```

The first declaration group, consisting of just `MyRecord` now cannot see `getRecordFoo`, and in case you need it, you'll be forced to move all the code that uses `getRecordFoo` into the second declaration group, after `makeLenses ''MyRecord`.


## Quotation

Haskell AST that TH can build and manipulate is not easy to work with at all. 
Manual construction of AST is tedious and error-prone - it is easy to produce an AST of a correct shape that does not compile. However, using *quotation* we can get a hold of the AST of arbitrary Haskell code.

These are types of quotations enabled by `TemplateHaskell`:

Thing produced  | Quotation syntax     | Type
----------------|----------------------|--------------
Declaration     | [d| … |]             | Q [Dec]
Expression      | [e| … |]  or [| … |] | Q Exp
Typed exp       | [|| … ||]            | Q (TExp a)
Pattern         | [p| … |]             | Q Pat
Type            | [t| … |]             | Q Type

Since most of the time we work with expressions, the quote syntax `[| … |]` is equivalent to `[e| … |]`.

We need several different quoters because the same code may mean different things is different contexts:

```hs
-- a pattern
>>> runQ [p| Just x |]
ConP GHC.Base.Just [VarP x_0]

-- an expression
>>> runQ [e| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)
-- same as:
>>> runQ [| Just x |]
AppE (ConE GHC.Base.Just) (UnboundVarE x)
```

Not only quotation can be used to quickly discover representation of a piece of Haskell code, it can be used in place of manually constructed ASTs:

```hs
myFunc :: Q Exp
myFunc = [| \x -> x + 1 |]
```

Great thing about quoters is that we can actually use splicing inside them:

```hs
add2 :: Q Exp
add2 = [| $myFunc . $myFunc |]
```

This way we can write the code we want to generate almost as usual, using splicing just to vary the pieces of code that need to change algorithmicly (as of 8.2.2, splicing of declarations inside declaration quoters doesn't work yet).

```hs
>>> $add2 10
12

>>> runQ add2
InfixE
  (Just (LamE [VarP x_2]                        -- lambda
        (InfixE (Just (VarE x_2))
                (VarE GHC.Num.+)
                (Just (LitE (IntegerL 1))))))
  (VarE GHC.Base..)                             -- composition
  (Just (LamE [VarP x_3]                        -- lambda
        (InfixE (Just (VarE x_3))
                (VarE GHC.Num.+)
                (Just (LitE (IntegerL 1))))))
```

## Typed expressions

Quotation for typed expressions is a bit special: it is the only way to create values of the type `TExp a`, i.e. it's the introduction form for `TExp`. So the compiler can ensure that the phantom type always corresponds to what's inside. Let's rewrite `myFunc` using quotation for typed expression splices:

```hs
myFuncTyped :: Q (TExp a)
myFuncTyped = [|| \x -> x + 1 ||]
-- ERROR: Couldn't match type a with Integer -> Integer

-- so we use that type:
myFuncTyped :: Q (TExp (Integer -> Integer))
myFuncTyped = [|| \x -> x + 1 ||]
```

GHC doesn't yet support impredicative polymorphism so returning something polymorphic is not possible:

```hs
myFuncTyped :: Q (TExp (Num a => a -> a))
myFuncTyped = [|| \x -> x + 1 ||]
-- ERROR: Illegal qualified type: Num a => a -> a
--        GHC doesn't yet support impredicative polymorphism
```

**Impredicative polymorphism** occurs when you try to replace a polymorphic variable with an expression which itself is polymorphic, i.e. contains a `forall`. Above, there is an implicit `forall` before the `Num a` constraint.

There is a special syntax for splicing of typed expressions. 
Let's write a typed version of `add2`:

```hs
add2Typed :: Q (TExp (Integer -> Integer))
add2Typed = [|| $$myFuncTyped . $$myFuncTyped ||]
```

>Normal splices cannot be used in quotations for typed exps and vice versa - typed splices cannot be used in quotations for untyped exps.

This is way we had to start by writing a typed version of `myFunc`.

When using the *double dollar syntax*, the compiler will make sure that we're splicing our typed expression in a correct context, so there won't be type errors.

Apart from splicing, using `unType` eliminates a value of type `TExp a`

```hs
unType :: TExp a -> Exp
```

More info about typed expressions can be found in this blog post:
https://www.cs.drexel.edu/~mainland/2013/05/31/type-safe-runtime-code-generation-with-typed-template-haskell/
