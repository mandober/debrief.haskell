# Pattern Matching locations



- in top-level binding
- in LHS of function definitions
- case...of
- let...in
- where

Places where the pattern matching is available are the places where potential bindings may occur. The usual places where PM are almost always found are in function definitions. They may even take place at the module's top level. Some contexts that enable PM are introduced with keywords such as `case-of`, `let-in`, `where`.


```hs
-- irref, top-level, let-in expr,
x = expr    -- x := expr

-- (refutable pattern matches)
fun <pat> = ...
-- (irrefutable pattern matches)
fun x     = ...
fun _     = ...
```




pattern    | expr          | refu | resulting bindings
-----------|---------------|------|-------------------------
x          | expr          | irr  | x := expr
_          | expr          | yes  | none
1          | 1             | yes  | none (matched literal)
(x:y)      | [1,2]         | yes  | x ← 1, y ← [2]
(x:y)      | [[1,2]]       | yes  | x ← [1,2], y ← []
(x:y)      | ["abc"]       | yes  | x ← "abc", y ← []
(x:y)      | "olemiss"     | yes  | x ← 'o', y ← "lemiss"
(1:x)      | [1,2]         | yes  | x ← [2]
(1:x)      | [2,3]         | no   |
`(x:_:_:y)`| [1,2,3,4,5,6] | yes  | x ← 1, y ← [4,5,6]
[]         | []            | yes  | none (matched literal)
[x]        | ["Cy"]        | yes  | x ← "Cy"
[1,x]      | [1,2]         | yes  | x ← 2
[x,y]      | [1]           | no   |
(x,y)      | (1,2)         | yes  | x ← 1, y ← 2




## Examples

```hs
-- Pattern matching at top-level
(a, b) = (1, 2)
[a, b] = [1, 2]

-- Typically PM happens in function definitions:
func (x:xs) = kill x : marry xs
```


The pattern `0` matches literal zero exclusively
