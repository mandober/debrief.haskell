# About continuations in Haskell

(About continuations in general: cs.debrief/plt/)

A continuation seems to be an overloaded term, referring to different things related to the continuation-passing style (CPS). The proper meaning of the term is best desribed in terms of a complex expression (complex, in that it is composed of subexpressions) and functions that containt them.

When evaluating a complex expression, the next subexpression to be evaluated is selected, driven by the grammar of the language, which include precedence rules, and using parenthesis to enforce grouping.
