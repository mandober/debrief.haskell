# Pattern Matching

Pattern matching is a language construct found mostly in FP. It is realized through a syntactical pattern against which the expressions (values) are matched and possibly bound. It is often, exceedingly generously, compared to the ancient runic keyword that had been known as "switch" or "case", even though it was confided to only perform value-matching.

Pattern matching provides high level of expressiveness and allows many kinds of scenarios, from setting up simple but effective patterns to overly complicated ones. It also offers the possibility to capture (i.e. bind) the matched expression, as a whole and/or any of its parts. The same amount of freedom that a language grants when defining a type, is available when performing pattern matching against the value of that type. Both a value and its type may be deconstructed to any level needed.

The types are matched against their data constructors; when pattern matching, Haskell, being a lazy language, will evaluate the expression just enough to resolve data constructor(s) of an expression, as they are essential for PM.

## Refutable and irrefutable PM

- refutable
- irrefutable

There are two kinds of (pattern) matches depending on the successfull matching (and potential binding) of a given pattern, refutable and irrefutable.

Matching may be forced (lazy/strict evaluation)


## PM Sites
- top-level binding
- function definition
- case...of
- let...in
- where

Places where the pattern matching is available are the places where potential bindings may occur. The usual places where PM are almost always found are in function definitions. They may even take place at the module's top level. Some contexts that enable PM are introduced with keywords such as `case-of`, `let-in`, `where`.


Pattern   | Expression    | Succeeds?  | Bindings
----------|---------------|------------|-------------------------
_         | <any>         | yes        | none
1         | 1             | yes        | none (matched literal)
x         | 1             | yes        | x ← 1
(x:y)     | [1,2]         | yes        | x ← 1, y ← [2]
(x:y)     | [[1,2]]       | yes        | x ← [1,2], y ← []
(x:y)     | ["olemiss"]   | yes        | x ← "olemiss", y ← []
(x:y)     | "olemiss"     | yes        | x ← 'o', y ← "lemiss"
(1:x)     | [1,2]         | yes        | x ← [2]
(1:x)     | [2,3]         | no         |
(x: : :y) | [1,2,3,4,5,6] | yes        | x ← 1, y ← [4,5,6]
[]        | []            | yes        | none (matched literal)
[x]       | ["Cy"]        | yes        | x ← "Cy"
[1,x]     | [1,2]         | yes        | x ← 2
[x,y]     | [1]           | no         |
(x,y)     | (1,2)         | yes        | x ← 1, y ← 2


## Examples


```hs
-- Top-level binding PM
(a, b) = (1, 2)
[a, b] = [1, 2]

-- Typically PM happens in function definitions:
func (x:xs) = kill x : marry xs
```


The pattern `0` matches literal zero exclusively



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
