# Literal values

Primitive flat types have literal forms
- Integral types
  - Int
  - Integer
  - Word
- Floating types
  - Float
  - Double
- Char
- `()` (unit)

Primitive compound types with literal forms
- list, [1,2,3] i.e. `1:2:3:[]`
- String, `"abc"` i.e. `['a', 'b', 'c']` i.e. `'a' : 'b' : 'c' : []`
- Text
- function types, lambdas, `\a -> a`
- tuples
  - pair, `(1, 'c')`
  - triple, `(1, 'c', 0.312)`

Integral literals
- may be writen in different bases
  - binary, `0b0000_0000`
  - octal, `0o77`
  - decimal (default), `42`
  - hexadecimal, `0xff`
- using literal negative numbers, `-1`
  - with *NegativeLiterals* pragma
  - *LexicalNegation*
  - *NumDecimals*
- using scientific notation
  - `0.31e4 == 0.31 * 10 ^ 4`
  - `0.31e-4 == 0.31 * 10 ^^ -4`
- using `_` as a digits separator, `1_000_000`




*:show language*
-XDataKinds
-XDefaultSignatures
-XDerivingStrategies
-XDerivingVia
-XDisambiguateRecordFields
-XBlockArguments
-XDuplicateRecordFields
-XFunctionalDependencies
-XGADTs
-XTypeFamilyDependencies
-XApplicativeDo
-XLambdaCase
-XLexicalNegation
-XLiberalTypeSynonyms
-XMonadComprehensions
-XMonoLocalBinds
-XMultiWayIf
-XNegativeLiterals
-XNondecreasingIndentation   (No-)
-XNumDecimals
-XPartialTypeSignatures
-XPatternSynonyms
-XQuantifiedConstraints
-XOverloadedRecordDot
-XOverloadedRecordUpdate
-XRecordWildCards
-XRoleAnnotations
-XNoStarIsType               (No-)
-XTypeFamilies
-XUnicodeSyntax
-XViewPatterns

*:showi language* = :show language +
-XExplicitNamespaces
-XExtendedDefaultRules
-XNoMonomorphismRestriction  (No-)
