# 2.3 Comments

https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-170002.3

* Comments are valid whitespace.

* A *line comment* begins with a sequence of 
  two or more consecutive dashes and 
  extends to the following newline.

* The sequence of dashes must not form part of a legal lexeme. 
  For example, `-->` or `|--` do not begin a comment, 
  because both are legal lexemes; 
  however `--foo` does start a comment.

* A *nested comment* begins with `{-` and ends with `-}`. 
  No legal lexeme starts with `{-`; 
  hence, `{---` starts a nested comment despite the trailing dashes.

* Nested comments are also used for *compiler pragmas*.

* If some code is commented out using a nested comment, 
  then any occurrence of `{-` or `-}` 
  within a string or 
  within an end-of-line comment 
  in that code will interfere with the nested comments.

* The comment itself is not lexically analysed.

* The first unmatched occurrence of the `-}` terminates the nested comment.

* Nested comments may be nested to any depth: 
  any occurrence of the `{-` within the nested comment
  starts a new nested comment, terminated by `-}`. 
  Within a nested comment, each `{-` is matched 
  by a corresponding occurrence of `-}`.

* In a line comment, the character sequences `{-` and `-}` 
  have no special significance, and, in a nested comment, 
  a sequence of dashes has no special significance.
