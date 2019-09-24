# Lazy evaluation

**Evaluation strategies** are used by programming languages to determine when to evaluate the arguments of a function call and what kind of value to pass to the function. The evaluation strategy is specified by the language specification, not by the implementation.

Many modern languages have converged on a call-by-value or call-by-reference evaluation strategies, although some have combined several notions of parameter passing with syntax that denotes specific strategies. Purely functional languages like Haskell, as well as non-purely functional languages like R, use *call-by-need*.

**Lazy evaluation** is Haskell's evaluation strategy. It means that an expression is not evaluated when it is bound to a variable, but its evaluation is deferred until its result is needed by other computation. As a consequence, arguments are not evaluated before they are passed to a function, only when their values are actually used.

Technically, lazy evaluation means *call-by-name with call-by-sharing*, which is sort of opposite of eager evaluation.

Lazy evaluation is part of *operational semantics* i.e. how a program is evaluated. The counterpart in *denotational semantics*, i.e. what a program computes, is called non-strict semantics.

**Non-strict semantics** allows one to bypass undefined values (e.g. results of infinite loops) and in this way allows processing of formally infinite data.

While lazy evaluation has many advantages, its main drawback is that memory consumption is hard to predict because, while expressions, like `2+2 :: Int` and `4 :: Int`, both denote the same value, they may use very different amounts of memory. An extreme example would be the infinite list `1 : 1 : 1 ...` and the expression `let x = 1:x in x`. The latter is represented as a cyclic graph, and takes only finite memory, but its denotation is the former infinite list.
