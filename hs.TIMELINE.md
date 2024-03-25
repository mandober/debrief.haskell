# Haskell TIMELINE

https://gitlab.haskell.org/ghc/ghc/-/wikis/language-pragma-history


- 1964 `where` and `where rec` expressions by Peter Landin in "The mechanical evaluation of expressions"[^1]
- 1969 `let` and `let rec` expressions in LCF language by Dana Scott[^2] in "A type-theoretic alternative to CUCH, ISWIM, OWHY" (unpublished)
- 1969 *Pattern matching* invented by Rod Burstall
- 1970 First impl of pattern matching, Fred McBride's LISP variant

- 1988 Control delimiters of delimited continuations, Matthias Felleisen
- 1989 Type classes, Wadler & Blott, POPL '89
- 1989 John Hughes "Why functional programming matters?"

- 1990 Intro of monads to FP, Philip Wadler (concept by E.Moggi)
- 1990 `Haskell` Report version 1.0, April 1st, 1990
- 1991 The notion of a singleton type was first put forward by S.Hayashi in "Singleton, union and intersection types for program extraction"
- 1998 H. Xi and F. Pfenning use singletons to simulate dependent types in "Eliminating array bound checking through dependent types" in ML
- 1999 "Dependent types in practical programming", H. Xi and F. Pfenning

- 2000 Functional dependencies, M. Jones (ESOP '00)
- 2000 Crary, Weirich used a kind-indexed definition to create singletons for arbitrary program values, "Resource bound certification"
- 2005 `OCaml`, Leroy et al.
- 2005 Data families (Chakravarty et al., POPL '05)
- 2005 Type families (Chakravarty et al., ICFP '05)
- 2005 C. Chen and H. Xi. preserving the phase-distinction via singleton types in "Combining programming with theorem proving"
- 2005 "Associated type synonyms", Chakravarty, Keller, Peyon Jones
- 2005 Sheard et al. showed how combining rich kinds with GADTs can yield dependently typed
- 2006 GADTs (Peyton Jones et al., ICFP '06)
- 2007 Kiselyov and Shan used a variant of singleton types to provide extra static guarantees

- 2012 Datatype promotion (Yorgey et al., TLDI '12)
- 2012 "Dependently Typed Programming with Singletons" (Eisenberg & Weirich, HS '12)
- 2013 `Type : Type` (Weirich et al., ICFP '13)
- 2014 Promotion of functions
- 2014 Closed type families (Eisenberg et al., POPL '14)
- 2014 Promotion of functions
- 2015 GADT pattern checking (Karachalias et al., ICFP '15)
- 2015 Injective type families (Stolarek et al., HS '15)
- 2016 Type application (Eisenberg et al., ESOP '16)
- 2016 New new Typeable (Peyton Jones et al., Wadlerfest '16)
- 2016 Pattern synonyms (Pickering et al., HS '16)
- 2017 Quantified class constraints (Bottu et al., HS '17)


[^1]: A closely related `where` clause, with its recursive variant `where rec`, appeared in Peter Landin's "The mechanical evaluation of expressions" 1964.

[^2]: 1969 Dana Scott's LCF language was a stage in the evolution of lambda calculus into modern FPL. LCF introduced the `let` expression, which has appeared in most FPLs since; Scheme, ML, and more recently Haskell have inherited `let` expressions from LCF. `let` and `let rec` expressions in LCF language by Dana Scott, described in his unpublished paper "A type-theoretic alternative to CUCH, ISWIM, OWHY".
https://en.wikipedia.org/wiki/Programming_Computable_Functions
