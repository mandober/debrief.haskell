# Haskell TIMELINE

https://gitlab.haskell.org/ghc/ghc/-/wikis/language-pragma-history

* 1960's
- 1964 `where` and `where rec` expressions by Peter Landin in 
  "The mechanical evaluation of expressions"
- 1969 `let` and `let rec` expressions in LCF language by Dana Scott in 
  "A type-theoretic alternative to CUCH, ISWIM, OWHY" (unpublished)
- 1969 Rod Burstall invents *pattern matching*
- 1970 first impl of *pattern matching* in Fred McBride's LISP variant

* 1980's
- 1989 type classes; P.Wadler, Blott

* 1990's
- 1990 Haskell Report version 1.0, April 1st, 1990
- 1991 The notion of a singleton type was first put forward by S.Hayashi in "Singleton, union and intersection types for program extraction"
- 1998 H. Xi and F. Pfenning use singletons to simulate dependent types in "Eliminating array bound checking through dependent types" in ML
- 1999 H. Xi and F. Pfenning "Dependent types in practical programming"

* 2000's
- 2000 functional dependencies, M. Jones
- 2000 Crary, Weirich used a kind-indexed definition to create singletons for arbitrary program values "Resource bound certification"
- 2005 data families by Chakravarty et al.
- 2005 type families by Chakravarty et al.
- 2005 C. Chen and H. Xi. preserving the phase-distinction via singleton types in "Combining programming with theorem proving"
- 2005 Chakravarty, Keller, Peyon Jones "Associated type synonyms"
- 2005 Sheard et al. showed how combining rich kinds with GADTs can yield dependently typed
- 2006 GADTs by Peyton Jones et al.
- 2007 Kiselyov and Shan used a variant of singleton types to provide extra static guarantees

* 2010's
- 2012 datatype promotion by Yorgey et al.
- 2012 Eisenberg, Weirich: "Dependently Typed Programming with Singletons"
- 2013 `Type :: Type` by Weirich, Eisenberg, et al.
- 2014 promotion of functions
- 2014 closed type families by Eisenberg et al.
- 2015 GADT pattern checking by Karachalias et al.
- 2015 injective type families by Stolarek et al.
- 2016 type application by Eisenberg et al.
- 2016 new new Typeable by Peyton Jones et al.
- 2016 pattern synonyms by Pickering et al.
- 2017 quantified class constraints by Bottu et al.

- 2014 ʜs: promotion of functions
- 2012 ʜs singletons in "Dependently Typed Programming with Singletons"
- 1990 Intro monads to FP by Philip Wadler (refining the concept by E.Moggi)
- 1990 `Haskell` Report version 1.0, April 1st, 1990
- 1989 John Hughes "Why functional programming matters?"
- 1988 ᴘʟᴛ Control delimiters of delimited continuations by Matthias Felleisen




• • •

A brief history of Haskell types
- type classes (Wadler & Blott, POPL '89)
- functional dependencies (Jones, ESOP '00)
- data families (Chakravarty et al., POPL '05)
- type families (Chakravarty et al., ICFP '05)
- GADTs (Peyton Jones et al., ICFP '06)
- datatype promotion (Yorgey et al., TLDI '12)
- singletons (Eisenberg & Weirich, HS '12)
- Type :: Type (Weirich et al., ICFP '13)
- closed type families (Eisenberg et al., POPL '14)
- GADT pattern checking (Karachalias et al., ICFP '15)
- injective type families (Stolarek et al., HS '15)
- type application (Eisenberg et al., ESOP '16)
- new new Typeable (Peyton Jones et al., Wadlerfest '16)
- pattern synonyms (Pickering et al., HS '16)
- quantified class constraints (Bottu et al., HS '17)

• • •

* 1969 Dana Scott's LCF language was a stage in the evolution of lambda calculus into modern FPL. LCF introduced the `let` expression, which has appeared in most FPLs since; Scheme, ML, and more recently Haskell have inherited `let` expressions from LCF. `let` and `let rec` expressions in LCF language by Dana Scott, described in his unpublished paper "A type-theoretic alternative to CUCH, ISWIM, OWHY".

https://en.wikipedia.org/wiki/Programming_Computable_Functions

* A closely related `where` clause, with its recursive variant `where rec`, appeared in Peter Landin's "The mechanical evaluation of expressions" 1964
