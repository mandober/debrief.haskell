# 4.1.3 Syntax of Class Assertions and Contexts

https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-660004.1.3

```js
context ::= class
          | ( class₁ , …, classₙ )                  (n ≥ 0)
class   ::= qtycls tyvar
          | qtycls ( tyvar atype₁ … atypeₙ )        (n ≥ 1)
qtycls  ::= [ modid . ] tycls
tycls   ::= conid
tyvar   ::= varid
```

* A **class assertion** has form `qtycls tyvar`, and indicates the membership of the type `tyvar` in the class `qtycls`.

* A class identifier begins with an uppercase letter.

* A **context** consists of zero or more class assertions, and has the general form `(C₁ u₁, …, Cₙ uₙ`) where `C₁, …, Cₙ` are class identifiers, and each of the `u₁, …, uₙ` is either a type variable, or the application of type variable to one or more types. The outer parentheses may be omitted when n = 1.

* In general, we use `cx` to denote a context and we write `cx => t` to indicate the type `t` restricted by the context `cx`.

* The context `cx` must only contain type variables referenced in `t`.

* For convenience, we write `cx => t` even when the context `cx` is empty, although in this case the concrete syntax contains no `=>`.

```hs
id :: () => a -> a                      -- the empty context (allowed)

show :: Show a => a -> String           -- n = 1 so no parens
show :: (Show a) => a -> String         -- but parens are always ok

toSet :: (Num a, Ord a) => a -> String  -- a single cxt, or, it can be
toSet :: Num a => Ord a => a -> String  -- split in multiple ctx is ok
```
