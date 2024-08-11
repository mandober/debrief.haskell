# Parsing in Haskell

A parser is a function like any other - it takes some input and returns some output (how insightful).

A parser is any function that processes (parses) the input, producing the output in the suitable format. The *input* is a sequence of symbols, usually a list of `Char`s, aka `String`. This is convenient as we can match the input and see whether it is `[]` aka `""` or `(x:xs)`. The *output*, for now, will be a list because it will allow us to use the empty list as an error signal. So the return value is the empty list in case of failure, and the successful payload will also be a list with the successfully parsed character in the first position and the rest of the input in second.

```hs
parser :: String -> (Char, String)
parser :: String -> [Char, String]
parser :: String -> [(Char, String)]
parser :: String -> Maybe (Char, String)

-- in fact, let's delay the decision of the input and output formats
type Input f c = f c
-- f = [], c = Char, then f c = [Char] = String
type Output f c i = f c i
-- f = (,), c = Char, i = String, then f c i = (,) Char String = (Char, String)

parser :: i -> f c i
parser :: Input f c -> Output f c i
```


(...)

However, instead of having a parser function take all parameters directly, we'll complicate things by defining a new data type, `Parser`, that will abstract the common parameters. Namely, the signature of parsing functions ends like this:

```hs
parser :: ... -> [Char] -> (Char, [Char])
parser :: ... -> String -> (Char, String)
parser :: ... -> [a]    -> (a   , [a]   )

newtype Parser a = Parser ([a] -> (a, [a]))
```

This type abstract "the end of signtures" of parser functions. Now individual parser functions can take whatever they need and return the `Parser` type. This *moves the last input parameter to the return type*. A function returning the `Parser` type will take its own arguments first (e.g. `x` and `y`) - let's have it take own args on the left-hand side of the equals sign:

```hs
parser1 :: x -> y -> Parser
parser1 x y = …
```

so the right-hand side is entirely dedicated to constructing the value, of the `Parser` type, that it must return.

But the `Parser` type is actually a function that has abstracted "the end of parsing functions' signatures", i.e. the last input type and the actual return type.

Thus, the first thing we need to do on the right-hand side of the equation is to place the `Parser` data ctor. That is, we wrap the entire right-hand side of the equation in the `Parser` data ctor. And instead of parenthesizing the rest of the expression, we use `$`.

```hs
parser1 :: x -> y -> Parser
parser1 x y = Parser $ …
```

Since the `Parser` type is actually a function that has abstracted "the end of parsing functions' signatures" (i.e. the last input type and the actual return type), we must introduce a lambda in order to bind the input stream (i.e. the last input type in the original parsing fucntions). Because, the `Parser` type has abstracted the last part of the signature, we must bind the arguments it "manages" via the lambdas on the right-hand side. Since the `Parser` type has "captured" one input parameter (not two or more), we have the lambda bind one argument - this argument is the input stream we need to parse.

>This is a clumsy first draft explanation of how the abstraction of a trailing part of a function signature, into a new data type, influences the binding of the captured (abstracted) input parameters. And the juggling with un/wrapping we must do in order to replicate the parsing fucntions' original intentions.

```hs
parser1 :: x -> y -> Parser
parser1 x y = Parser \ inp -> …
```

## TL/DR

```hs
{-| step 1 -}

-- This parsing functions has 2 input params: the 1st param is the char that is to be parsed; the 2nd param binds the input stream. It returns a 2-element list with the parsed char in the first position, and the rest of the unparsed input in the second position. In case of any error, it returns the empty list.
original1 :: Char -> String -> [Char, String]

-- This parsing function applies a predicate against the first char of the input stream, returning it on success.
original2 :: (Char -> Bool) -> String -> [Char, String]

{-| step 2 -}

-- Note that the sigs of all parsing functions end in the same way.
-- We abstract that part of the sig into its own type.
newtype Parser = Parser { runParser :: String -> [Char, String] }




original x y = Parser \ inp -> …
```
