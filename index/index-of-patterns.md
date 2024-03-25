# Haskell :: Index :: Patterns

Index of patterns:
- Properties of patterns
  - refutable pattern
  - irrefutable pattern
  - binding pattern
  - nonbinding pattern
  - literal pattern
  - compound pattern
  - as-pattern
  - strict pattern
  - lazy pattern
  - extra lazy pattern
- Opposing properties
  - refutable vs refutable patterns
  - binding vs nonbinding patterns
  - strict vs lazy vs extra lazy patterns

Patterns
- as-pattern
  - binding as-pattern at the top-level
- pattern synonyms
  - unidirectional
  - bidirectional
- refutable patterns
  - literal pattern are nonbinding
    - `'a'`, `'\n'`; `"and now"`; `4`, `0.341`; `[]`
- data ctors
  - list
    - literal
      - []
      - ['c']
      - [1,2,3]
      - "pattern"
      - multiple literal patterns:
        - True 3 "done"
        - "pattern" "matching"
    - ls
    - [x]
    - [x,y]
    - (x:xs)
    - ls@(x:xs)
    - ls@(x:_)
    - (x:xs:ys)
    - ((a,b):xs)
    - xs@ys@(p@(a,b):as)
    - Creating vars by deconstructing a list.
      [a,b,c,d] = [1, 'a', "over", True]
      Type signature stated separately for each var.
  - pair
    - p
    - p@(a,b)
    - p1@p2@(a,b)
    - (a,b)
    - (a,_)
    - (_,b)
    - (_,_)
    - (a,(b,c))

- irrefutable patterns
  - as-pattern, `e@PAT`, only the `e` pattern
    - e.g. `filter f e@(x:xs)`
  - top-level binding pattern, `fib@PAT` (fn name and as-pattern is the same)
  - variable, `x`
    - multiple as-patterns:
      - double as-pattern binding, `xs@ys = [1,2,3]`
      - triple as-pattern binding, `xs@ys@zs = [1,2,3]`
      - then `xs == ys == zs`
      - multiple as-pattern binding plus a normal pattern:
        `xs@ys@zs@(a:as) = [1,2,3]`
  - discard-ignore-whatever pattern, `_`
    - but: value typed hole, `_1`, `_a` (in value context)
    - but: type typed hole, `_` (in type context)
      - also: partially specified type, `Maybe [_]`
