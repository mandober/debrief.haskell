# Defining functions

Unsurprisingly, functions are the main means of abstraction in FP, so Haskell has several ways of declaring a function.

## Declaration vs definition

- language items
- function declaration
- function definition
- function signature
- forward declaration
- forward implementation
- instantiation
- function prototype
- function formal parameters
- function arguments
- function head
- function body
- function application
- trivial function application (tagging)


Since Haskell doesn't require *forward declarations* (you can declare language items in any order, so you can reference an item before it is declared), there is no real difference between declarations and definitions, and both terms are often used interchangibly. Haskell Report does seems to use the term "declaration" consistently, but then again the term "definition" is not used at all in the same context.

PLs that insist on forward declarations, like Agda, require you to first specify the signature of an language item before you can reference it (usually by name), provided the definition of that item is specified later in the code. Each PL has a set of rules regarding what exactly qualifies as a valid forward declaration, which in case of functions could be called a function's prototype (and it is their type signature). Later in the source code, when you do give the definition, additional rules determine its shape, i.e. which things may (or must) be left out from the signature (because they were already specified in the forward declaration), and whether the signature is required at all.


As far as functions are concerned, introducing a function should be called a function declaration.

A *function signature* is what other languages might call a *function prototype* because it describes the function completely (as far as compiler is concerned), so it would qualify as a *forward declaration*.


that is, a PL that requires declare-before-use would probably consider a type signature 

; the number and type of input arguments and the return type, along with the function's name is all the information a compiler needs for a valid forward decalaration - it is not concerened with the actual values of the types involved as it can completely determine the size requirements from the types (at least in the general case).


of language items like data type and function declarations.
