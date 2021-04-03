# Name Binding

* **View patterns** extend our ability to pattern match on variables by also allowing us to *pattern match on the result of function application*. Enabled by `ViewPatterns` language pragma.

* View patterns are somewhat like *pattern guards* that can be nested inside of other patterns. They are a convenient way of pattern-matching against values of abstract types. For example, in a PL implementation, we might represent the syntax of the types of the language as follows:

```hs
type Typ
data TypView = Unit | Arrow Typ Typ
view :: Typ -> TypView
-- additional operations for constructing Typ's ...
```

The representation of `Typ` is held abstract, permitting implementations to use a fancy representation (e.g. hash-consing to manage sharing). Without view patterns, using this signature is a little inconvenient:

```hs
size :: Typ -> Integer
size t = case view t of
    Unit        -> 1
    Arrow t1 t2 -> size t1 + size t2
```










## Ref

24 Days of GHC Extensions: View Patterns
https://ocharles.org.uk/posts/2014-12-02-view-patterns.html

GHC users guide: view patterns
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#view-patterns

Reviewing View Patterns
http://neilmitchell.blogspot.com/2009/11/reviewing-view-patterns.html

View patterns: lightweight views for Haskell
https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns

Alternative view patterns: lightweight views for Haskell
https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns-alternative
