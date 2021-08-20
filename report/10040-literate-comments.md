# 10.4 Literate comments


The *literate comment* convention, first developed by Richard Bird and Philip Wadler for Orwell, and inspired in turn by Donald Knuth's "literate programming", is an alternative style for encoding Haskell source code.

The literate style encourages comments by making them the default. A line in which `>` is the first character is treated as part of the program; all other lines are comments.

The program text is recovered by taking only those lines beginning with `>`, and replacing the leading `>` with a space.

Layout and comments apply exactly as described [here](10030-layout.md).

To capture some cases where one omits an `>` by mistake, it is an error for a program line to appear adjacent to a non-blank comment line, where a line is taken as blank if it consists only of whitespace.

By convention, ".hs" indicates a usual Haskell file and ".lhs" indicating a literate Haskell file.

Using this style, a simple factorial program would be:

```lhs
This literate program prompts the user for a number
and prints the factorial of that number:

> main :: IO ()
> main = do putStr "Enter a number: "
>           l <- readLine
>           putStr "n!= "
>           print (fact (read l))

This is the factorial function:

> fact :: Integer -> Integer
> fact 0 = 1
> fact n = n ⋆ fact (n-1)
```

An alternative style of literate programming is particularly suitable for use with the *LaTeX* text processing system. In this convention, only those parts of the literate program that are entirely enclosed between `\begin{code}` and `\end{code}` delimiters are treated as program text; all other lines are comments. 

More precisely:
- Program code begins on the first line following a line that begins `\begin{code}`
- Program code ends just before a subsequent line that begins `\end{code}` (ignoring string literals, of course).

It is not necessary to insert additional blank lines before or after these delimiters, though it may be stylistically desirable. For example,

```latex
\\documentstyle{article}

\\begin{document}

\\chapter{Introduction}

This is a trivial program that prints the first 20 factorials.

\\begin{code}
main :: IO ()
main =  print \[ (n, product \[1..n\]) | n <- \[1..20\]\]
\\end{code}

\\end{document}
```

This style uses the same file extension ".lhs".

It is not advisable to mix these two styles in the same file.
