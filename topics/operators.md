# Operators

Lambda calculus, the base of FPLs and Haskell, has no notion of infix functions as functions can only be in the prefix position. LC does define that the function application is left-associative and has higher precedence then function abstraction which is right-associative. Haskell keeps the rule about the highest precedence of function application, but introduces the possibility yo have functions in the infix position, and, moreover, allows for a user-definable fixity which creates a slew of problems for the parser as it proves to be very challenging to get right and implement consistently.

An operator is a function used in the infix position. In a more narrow sense, an operator is a symbolically-named function which is most naturally used in the infix position ("sym" in the further text, or just "operator"). In the most narrow sense, it's assumed an operator is binary; although that is the usual case, operators can have a higher arity.

A polyadic function with alphanumeric name ("alpha" in the further text) is most naturally used in the prefix position. The term "used" implies that alpha is to be applied to some arguments, and function application has the highest precedence (it is evaluated the first) of all.

Function application's fixity is pre-set at `-1`, an out-of-range value for fixity points, which are normally in the 0-10 (low to high) range. When used in its natural prefix position, alphas behave the same, regardless if some of them had their fixity adjusted - fixity of a function/operator has no effect when used prefix, only when used infix.

However, any polyadic function with alphanumeric name can also be used in the infix position, by enclosing its name in ticks. However, when used in the infix position their precedence lowers by a fixity point (it is lower then what it would enjoyed if it were used in its natural, prefix, position), and all alpha-functions have the same precedence when used in the infix position.

Any polyadic function with symbolic name can be used in the prefix position by enclosing its name in parenthesis. (...)

## Trouble with Fixity

> The problem is that a function/operator (with some defined fixity) bound to a parameter, that is then used in the infix position, looses its fixity information (both associativity and precedence).

```hs
-- Operator bound to a param that is used as infix looses fixity

-- looses associativity
e1 = 4 ^ 3 ^ 2                  -- 4^(3^2) = 4^9  ✔
e2 = (\f -> 4 `f` 3 `f` 2) (^)  -- (4^3)^2 = 4^6  ✘

-- looses precedence
e3 = 4 + 3 ^ 2                        -- 4+(3^2) = 13  ✔
e4 = (\g f -> 4 `g` 3 `f` 2) (+) (^)  -- (4+3)^2 = 49  ✘
```


## Fixity

- function application (prefix): infixr -1
- operator lacking fixity: infixl 9
- infix alpha application: infixl 9
- fixity of `::` e.g. `f x = x ↑ y ↑ z :: Int`
- fixity of `@Int`
- fixity of type-level operators, e.g. `~`, `~~` (lifted `++`, `:`, `[]`, etc.)

From the Haskell98 report
- infixr 9  ., !!
- infixr 8  ^, ^^, **
- infixl 7  *, /, `quot`, `rem`, `div`, `mod`
- infixl 6  +, -
- infixr 5  :, ++
- infix  4  ==, /=, <, <=, >=, >
- infixr 3  &&
- infixr 2  ||
- infixl 1  >>, >>=
- infixr 1  =<<
- infixr 0  $, $!, `seq`
