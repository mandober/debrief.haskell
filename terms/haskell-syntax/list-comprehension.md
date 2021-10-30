# List comprehensions

- list comprehension
- simplification rules
- monad comprehension



List comprehensions
- list comprehension is a list, so list ops are allowed: prepend, concat, etc.
- producer expression, single expression
- pipe separator
- local let bindings, none or more, comma separated
- generator, one or more, comma separated
- generator order matters, later gens may depend on previous
- predicate fn called guard, none or more, comma separated

`v1 : v2 : [ pe | llb1,llb2,... , g1,g2,... , p1,p2,... ] ++ [...]`
