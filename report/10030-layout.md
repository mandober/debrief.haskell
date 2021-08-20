# 10.3 Layout
10030-layout.md

[Section 2.7](2700-layout.md) gives an informal discussion of the layout rule. This section defines it more precisely.



The meaning of a Haskell program may depend on its layout. The effect of layout on its meaning can be completely described by adding braces and semicolons in places determined by the layout. The meaning of this augmented program is now layout insensitive.

The effect of layout is specified in this section by describing how to add braces and semicolons to a laid-out program. The specification takes the form of a function L that performs the translation. The input to L is:

*   A stream of lexemes as specified by the lexical syntax in the Haskell report, with the following additional tokens:
    *   If a let, where, do, or of keyword is not followed by the lexeme {, the token {n} is inserted after the keyword, where n is the indentation of the next lexeme if there is one, or 0 if the end of file has been reached.
    *   If the first lexeme of a module is not { or module, then it is preceded by {n} where n is the indentation of the lexeme.
    *   Where the start of a lexeme is preceded only by white space on the same line, this lexeme is preceded by < n > where n is the indentation of the lexeme, provided that it is not, as a consequence of the first two rules, preceded by {n}. (NB: a string literal may span multiple lines – Section [2.6](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch2.html#x7-200002.6). So in the fragment
        
          f = ("Hello \\    
                  \\Bill", "Jake")
        
        There is no < n > inserted before the \\Bill, because it is not the beginning of a complete lexeme; nor before the ,, because it is not preceded only by white space.)
        
*   A stack of “layout contexts”, in which each element is either:
    *   Zero, indicating that the enclosing context is explicit (i.e. the programmer supplied the opening brace). If the innermost context is 0, then no layout tokens will be inserted until either the enclosing context ends or a new context is pushed.
    *   A positive integer, which is the indentation column of the enclosing layout context.

The “indentation” of a lexeme is the column number of the first character of that lexeme; the indentation of a line is the indentation of its leftmost lexeme. To determine the column number, assume a fixed-width font with the following conventions:

*   The characters newline, return, linefeed, and formfeed, all start a new line.
*   The first column is designated column 1, not 0.
*   Tab stops are 8 characters apart.
*   A tab character causes the insertion of enough spaces to align the current position with the next tab stop.

For the purposes of the layout rule, Unicode characters in a source program are considered to be of the same, fixed, width as an ASCII character. However, to avoid visual confusion, programmers should avoid writing programs in which the meaning of implicit layout depends on the width of non-space characters.

The application L tokens \[\] delivers a layout-insensitive translation of tokens, where tokens is the result of lexically analysing a module and adding column-number indicators to it as described above. The definition of L is as follows, where we use “:” as a stream construction operator, and “\[\]” for the empty stream.

+

Note 1.

A nested context must be further indented than the enclosing context (n > m). If not, L fails, and the compiler should indicate a layout error. An example is:

  f x = let    
           h y = let    
    p z = z    
                 in p    
        in h

Here, the definition of p is indented less than the indentation of the enclosing context, which is set in this case by the definition of h.

Note 2.

If the first token after a where (say) is not indented more than the enclosing layout context, then the block must be empty, so empty braces are inserted. The {n} token is replaced by < n >, to mimic the situation if the empty braces had been explicit.

Note 3.

By matching against 0 for the current layout context, we ensure that an explicit close brace can only match an explicit open brace. A parse error results if an explicit close brace matches an implicit open brace.

Note 4.

This clause means that all brace pairs are treated as explicit layout contexts, including labelled construction and update (Section [3.15](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch3.html#x8-490003.15)). This is a difference between this formulation and Haskell 1.4.

Note 5.

The side condition parse-error(t) is to be interpreted as follows: if the tokens generated so far by L together with the next token t represent an invalid prefix of the Haskell grammar, and the tokens generated so far by L followed by the token “}” represent a valid prefix of the Haskell grammar, then parse-error(t) is true.

The test m∕ \= 0 checks that an implicitly-added closing brace would match an implicit open brace.

Note 6.

At the end of the input, any pending close-braces are inserted. It is an error at this point to be within a non-layout context (i.e.  m \= 0).

If none of the rules given above matches, then the algorithm fails. It can fail for instance when the end of the input is reached, and a non-layout context is active, since the close brace is missing. Some error conditions are not detected by the algorithm, although they could be: for example let }.

Note 1 implements the feature that layout processing can be stopped prematurely by a parse error. For example

is valid, because it translates to

        let { x = e; y = x } in e'

The close brace is inserted due to the parse error rule above.
