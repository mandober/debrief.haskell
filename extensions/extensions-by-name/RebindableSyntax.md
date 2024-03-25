# RebindableSyntax

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html

- ext:     RebindableSyntax
- implies: NoImplicitPrelude
- related: OverloadedStrings
           OverloadedLists
           OverloadedLabels
- extcat: syntax
- since: 7.0.1
- desc: Enable rebinding of a variety of usually-built-in operations.


## Summary

RebindableSyntax affects
- 

RebindableSyntax does not affect
- code generated from a deriving clause or declaration


## Intro

The literal number `1` is polymorphic, so normally, to get its meaning it is passed into the function `fromInteger` which is available as part of the `Prelude`, but explicitly that call is `Prelude.fromInteger 1` as specified by the Haskell Report. However, if you're importing a different Prelude in order to define your own numeric class hierarchy, for example, this is not want you'd want. You want your own function `fromInteger` to handle this, and it can be done with the RebindableSyntax extension.

RebindableSyntax causes the following *built-in syntax to refer to whatever is in scope, and not the `Prelude` versions*:

- An integer literal `368` now means `fromInteger (368 :: Integer)`, 
  rather than `Prelude.fromInteger (368 :: Integer)`.

- Fractional literals are handled in just the same way, 
  except the translation function is `fromRational (3.68 :: Rational)`.

- String literals are also handled the same way, 
  except that the translation is `fromString ("368" :: String)`.

- The equality test in an overloaded numeric pattern 
  uses whatever `==` is in scope.

- The subtraction operation, and the greater-than-or-equal test, 
  in n+k patterns (DEPRECATED) use whatever `-` and `>=` are in scope.

- Negation, e.g. `- (f x)`, means `negate (f x)`, 
  both in numeric patterns, and expressions.

- Conditionals like `if e1 then e2 else e3` mean `ifThenElse e1 e2 e3`. 
  However case expressions are unaffected.

- The do-notation is translated using whatever functions `>>=`, `>>`, `fail` 
  are in scope (not the Prelude versions). However, list comprehensions, `mdo` (recursive do-notation), and parallel array comprehensions remain unaffected.

- List notation, such as [x,y] or [m..n] can also be treated via rebindable syntax if you also add the `OverloadedLists` pragma.

- An overloaded label `#foo` means `fromLabel @"foo"`, 
  rather than `GHC.OverloadedLabels.fromLabel @"foo"`.

- Arrow notation uses whatever `arr`, `>>>`, `first`, `app`, `|||` and `loop` functions are in scope. But unlike the other constructs, the types of these functions must match the Prelude types very closely (details are in flux).


* https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html#recursive-do-notation
* https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html#arrow-notation
* https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_lists.html#overloaded-lists
* https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html#overloaded-labels



## Static semantics

In all cases (apart from arrow notation), the *static semantics* should be that of the desugared form, even if that is a little unexpected.

For example, the static semantics of the literal `42` is exactly that of `fromInteger (42 :: Integer)`; it's fine for `fromInteger` to have any of these types:
- fromInteger :: Integer -> Integer
- fromInteger :: forall a. (Foo a) => Integer -> a
- fromInteger :: (Num a) => a -> Integer
- fromInteger :: Integer -> Bool -> Bool

Be warned: this is an experimental extension, with fewer checks than usual. Use `-dcore-lint` to typecheck the desugared program - if *Core Lint* is happy you should be all right.


## 6.2.10.1. Things unaffected by RebindableSyntax

RebindableSyntax does not apply to any code generated from a deriving clause or declaration.

To see why, consider the following code:

```hs
{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
newtype Text = Text String

fromString :: String -> Text
fromString = Text

data Foo = Foo deriving Show


-- | This will generate code to the effect of:

instance Show Foo where
  showsPrec _ Foo = showString "Foo"
```

But because RebindableSyntax and OverloadedStrings are enabled, the "Foo" string literal would now be of type `Text`, not `String`, which `showString` doesn't accept! This causes the generated `Show` instance to fail to typecheck. It's hard to imagine any scenario where it would be desirable have RebindableSyntax behavior within derived code, so **GHC simply ignores RebindableSyntax entirely when checking derived code**.




## References

* GHC Docs: 6. Language extensions » 6.2. Syntax » 6.2.10. Rebindable syntax
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html

* 24 Days of GHC Extensions: Rebindable Syntax
https://blog.ocharles.org.uk/guest-posts/2014-12-06-rebindable-syntax.html
