# Pattern Matching

Pattern matching is a language construct found mostly in FP. It is realized through a syntactical pattern against which the expressions (values) are matched and possibly bound. It is soemytimes, exceedingly generously for the latter, compared to the ancient runic keywords "switch" or "case", although with clarification that the latter are confided strictly to value-matching.

Pattern matching provides high level of expressiveness and allows many kinds of scenarios, from setting up simple but effective patterns to very complicated ones.

Pattern matching also provides the possibility to bind (capture) the matched expression, in its entirety or in part.

The same amount of freedom that a language grants when defining a type, is available when performing pattern matching against the value of that type. Both a value and its type may be deconstructed to any level needed.

The types are matched against their *data constructors*; when pattern matching, Haskell, being a lazy language, will evaluate the expression just to their WHNF. Weak-head normal form reveals data ctors without eval all the way to the containing values (e.g. evaluating a expr that is a list until (:) and [] ctors are revealed, but not the list's values).

The data ctors are essential for pattern matching, so pattern-matching an expression sort of "unpacks" one level of type wrapping. The place where pMis done frequently is in multi-piece function definition, where each piece has a different pattern so an expr goes throught each, from the top down order, until it matches in which case the RHS of the piece is executed. If no match occurs, the exception is thrown becasue the PM has to be exhaustive.


## Refutable and irrefutable PM

There are two kinds of pattern matching depending on the successfull matching (and potential binding) of a given pattern: refutable and irrefutable.

Matching may be forced (lazy/strict evaluation)


## Pattern matching and binding modes

Pattern matching may be done in two modes: strict and non-strict.

```hs
-- non-strict pattern matching mode
mx >>= mk = mk $ unTy mx

-- strict pattern matching mode evals an expr until WHNF
(Ctor x) >>= mk = mk $ unTy mx
```

Although they amount to the same, there is a difference between these two approaches:
1. Irrefutable pattern binds the expression in *non-strict binding mode*
2. Pattern matching an expression uses the *strict mode binding*

Strict mode forces the evaluation of an expr to WHNF. In case of a data ctor, it means evaluating the expr/type just enough until the data ctor is revealed.



## Deprecated: n + k pattern

Haskell had a special pattern for pattern matching on the natural number that closely resembled mathematical patterns of the form `n+1` or `succ(1)` or, more generally, `n+k`

```hs
fact5 :: Int -> Int
fact5 0 = 1
fact5 (n+1) = (n+1) * fact5 n
```

The special pattern (n+1) only matches integer arguments that are at least 1, binding the variable `n` to the value that is one less than the value of the integer argument.


One interesting fact is that you can pattern match directly on `let` and `where` bindings. In that case, you can handle only one pattern, but it is useful when you know that only one kind of value can happen in a specific place.

Let's say you are sure at some point that the client you are working with is a company:

```hs
companyName :: Client -> Maybe String
companyName client = case client of
    Company name _ _ _ -> Just name
    _                  -> Nothing

-- Instead of this, not very clear, code:
...
let name = case companyName client of Just n -> n

-- You can write the following concise version:
...
let Just name = companyName client
```


## View patterns

- https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns
- http://neilmitchell.blogspot.com/2009/11/reviewing-view-patterns.html
- https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns-alternative


View patterns are a convenient way of pattern-matching against values of abstract types.

For example, when impl a PL, we might represent the syntax of the types of the language with:

```hs
type Typ
data TypView = Unit | Arrow Typ Typ
view :: Typ -> TypView
-- additional operations for constructing Typ's ...
```

The representation of `Typ` is held abstract, permitting implementations to use a fancy representation (e.g., hash-consing to manage sharing).
