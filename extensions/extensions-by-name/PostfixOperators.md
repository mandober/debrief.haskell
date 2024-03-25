# Postfix operators

## 6.2.11. Postfix operators

- ext:    PostfixOperators
- since:  7.10.1
- status: Included in GHC2021
- desc:   Allow the use of post-fix operators

The PostfixOperators extension enables a small extension to the syntax of *left operator sections* (e.g. `(x +)` with the *operator is on the right*), which allows you to define postfix operators.

The extension is this: for any expression `e` and operator `!`, the left section, `(e !)`, is equivalent (from the point of view of both type checking and execution) to the expression `((!) e)`.

The strict Haskell 98 interpretation is that the section is equivalent to

`(\y -> (!) e y)`

that is, the operator must be a function of two arguments. GHC allows it to take only one argument, and that in turn allows you to write the function postfix.

The extension does not extend to the left-hand side of function definitions; you must define such a function in prefix form.
