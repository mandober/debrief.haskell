# Rebindable Syntax

9.3.17. Rebindable syntax and the implicit Prelude import
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax


## No Prelude
- `NoImplicitPrelude`
- Since: 6.8.1
- *Don't import Prelude by default*
- GHC normally imports `Prelude.hi` files for you. So, e.g. the literal `1` means `Prelude.fromInteger 1`. To disable it use `-XNoImplicitPrelude` option and then import a Prelude of your own, just don't call it Prelude.


## Rebindable Syntax
- `RebindableSyntax`
- Since: 7.0.1
- implies: `NoImplicitPrelude`
- *Enables rebinding of a variety of usually-built-in operations*
- `RebindableSyntax` extension causes the following pieces of built-in syntax to refer to whatever is in scope (and not in the Prelude).

RebindableSyntax affects:
* Literals:
  - Integer literals:    `fromInteger (368::Integer)`
  - Fractional literals: `fromRational (3.68::Rational)`
  - String literals:     `fromString ("368"::String)`
* Arithmetic
  - (==) in overloaded numeric pattern uses (==) that's currently in scope
  - (-) and (>=) subtraction and GE, in n+k patterns use whatever is in scope
  - Negation, e.g. `- (f x)`, means `negate (f x)`, in numeric patterns, expr
* Conditionals (e.g. `if e1 then e2 else e3`, mean `ifThenElse e1 e2 e3`.
  - unaffected: case expressions
* `Do` notation is translated using `(>>=)`, `(>>)` and `fail` in scope.
  - unaffected: list comprehensions, mdo (recursive do-notation), parallel array comprehensions
* Arrow notation 
  - uses whatever `arr`, `(>>>)`, `first`, `app`, `(|||)`, `loop` are in scope.
  - unlike the other constructs, the types of these functions must match the Prelude types very closely. (Warning: details are still in the flux)
* List notation
  - use `-XOverloadedLists` to rebing, e.g. [x,y] or [m..n]
* Labels
  - overloaded label `#foo` means `fromLabel @"foo"`,    
    not `GHC.OverloadedLabels.fromLabel @"foo"`

In all cases (apart from arrow notation), the static semantics should be that of the desugared form, even if that is a little unexpected.

For example, the static semantics of the literal `368` is exactly that of `fromInteger (368::Integer)`; it's fine for `fromInteger` to have any of the types:
* fromInteger :: Integer -> Integer
* fromInteger :: forall a. Foo a => Integer -> a
* fromInteger :: Num a => a -> Integer
* fromInteger :: Integer -> Bool -> Bool

Be warned: this is an experimental facility, with fewer checks than usual.
- Use `-dcore-lint` to typecheck the desugared program. 
- If Core Lint is happy you should be all right.


## Unaffected things

`RebindableSyntax` does not apply to any code generated from:
- **deriving clause**
- **declaration**

To see why, consider the following code:

```hs
{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
newtype Text = Text String

fromString :: String -> Text
fromString = Text

data Foo = Foo deriving Show
```

This will generate code to the effect of:

```hs
instance Show Foo where
  showsPrec _ Foo = showString "Foo"
```

But because RebindableSyntax and OverloadedStrings are enabled, the "Foo" string literal would now be of type Text, not String, which showString doesn’t accept! This causes the generated Show instance to fail to typecheck. It’s hard to imagine any scenario where it would be desirable have RebindableSyntax behavior within derived code, so GHC simply ignores RebindableSyntax entirely when checking derived code.
