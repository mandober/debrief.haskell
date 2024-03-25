# Haskell family

## Template Haskell

https://wiki.haskell.org/Template_Haskell
https://en.wikipedia.org/wiki/Template_Haskell

Initially, it was an experimental language extension, called *Template Meta-Haskell*, but it eventually became integrated into the GHC-based Haskell language, where it is available today under the extension `TemplateHaskell`. It allows compile-time metaprogramming and generative programming by means of manipulating AST and "splicing" results back in. The abstract syntax is represented using ordinary Haskell data types and the manipulations are performed using ordinary Haskell functions.

Quasi-quotation brackets, `[q|..|]`, are used to get the AST for the enclosed expression and the splicing brackets `$(..)` are used to convert modified AST back to code.

As of GHC-6.10, Template Haskell provides support for user-defined quasi-quoters, which allows users to write parsers which can generate Haskell code from an arbitrary syntax. This syntax is also enforced at compile time.

For example, (1) using a custom quasi-quoter for regular expressions; (2) a common idiom is to quasi-quote an expression, perform some transformation on the expression and splice the result back into the program.

```hs
-- (1)
digitsFollowedByLetters = [$re| \d+ \s+ |]

-- (2)
result = $( transform [| input |] )
```
