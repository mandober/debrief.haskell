# Tackling the Awkward Squad in Haskell
Simon Peyton Jones, 2001

The awkward squad:
- MIO (monadic IO)
- CCR (Concurrency)
- EER (Errors, Exceptions)
- FFI (foreign libs call)

To write programs that are useful, the programmer must eventually confront *the Awkward Squad*, a range of un-beautiful but crucial issues, generally concerning interaction with the external world:
- Input and output
- Error detection and recovery; for example, perhaps the program should time out if something does not happen in time
- Concurrency, when the program must react in a timely way to independent input sources
- Interfacing to libraries or components written in other languages

The call-by-value (or strict) family of FPL have taken a pragmatic approach, adopting the strategy of IPL (imperative programming languages). To print something, there is a printChar function that has the side effect of printing a character. However, printChar isn't really a function anymore (because it has a side effect), but in practice this approach works ok, provided you specify the order of evaluation as part of the language design. This is what almost all PLs do, from FORTRAN and Java to iFPs (impure FPLs) like Lisp and SML.

Call-by-need (or lazy) languages (such as Haskell) wear a hair shirt because their evaluation order is deliberately unspecified.
