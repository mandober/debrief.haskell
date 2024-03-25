# Haskell tooling

* DrIFT Homepage
https://web.archive.org/web/20160527003939/http://repetae.net/computer/haskell/DrIFT/

DrIFT is a type sensitive preprocessor for Haskell. It extracts type declarations and directives from modules. The directives cause rules to be fired on the parsed type declarations, generating new code which is then appended to the bottom of the input file. The rules are expressed as Haskell code, and it is intended that the user can add new rules as required.

DrIFT automates instance derivation for classes that aren't supported by the standard compilers. In addition, instances can be produced in seperate modules to that containing the type declaration. This allows instances to be derived for a type after the original module has been compiled. As a bonus, simple utility functions can also be produced from a type.

https://web.archive.org/web/20160326234336/http://repetae.net/computer/haskell/DrIFT/drift.html
https://web.archive.org/web/20160326231127/http://repetae.net/computer/haskell/DrIFT/drop/


## Libs

* Tying_the_Knot
https://wiki.haskell.org/Tying_the_Knot

* Knuth-Morris-Pratt-in-Haskell
https://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell

Boolean
* https://web.archive.org/web/20160620000310/http://repetae.net/recent/out/Boolean.html
* https://web.archive.org/web/20160422103208/http://repetae.net/recent/src/hsdocs/

* GetOptions
https://web.archive.org/web/20160620000321/http://repetae.net/recent/out/GetOptions.html

* Doc
https://web.archive.org/web/20160620000218/http://repetae.net/recent/out/Doc.html

* HsLocale
https://web.archive.org/web/20160620000228/http://repetae.net/recent/out/HsLocale.html

* HsASA
https://web.archive.org/web/20160620000223/http://repetae.net/recent/out/HsASA.html

* ErrorLog
https://web.archive.org/web/20160501104242/http://repetae.net/recent/out/ErrorLog.html

* BubbleBabble
https://web.archive.org/web/20160620000213/http://repetae.net/recent/out/BubbleBabble.html

* GenUtil
https://web.archive.org/web/20160620000316/http://repetae.net/recent/out/GenUtil.html
https://web.archive.org/web/20160422093458/http://repetae.net/recent/src/hsdocs/GenUtil.html
This is a collection of random useful utility functions written in pure Haskell 98. In general, it trys to conform to the naming scheme put forth the haskell prelude and fill in the obvious omissions, as well as provide useful routines in general. To ensure maximum portability, no instances are exported so it may be added to any project without conflicts.

* JRegex
https://web.archive.org/web/20160620000153/http://repetae.net/computer/haskell/JRegex
This library provides an overloaded perl-like (=~) regular expression operator for haskell. Since haskell type classes allow routines to change their behavior based on the return type of values, this operator ends up being much more powerful than the perl version. This library also provides an interface to PCRE to use perl compatable regular expressions and a more advanced interface to the Posix regex matcher.

* classalias
https://web.archive.org/web/20160620000254/http://repetae.net/recent/out/classalias.html
This is a proposal for a language extension which will hopefully mitigate the issues holding back evolution of the standard prelude as well as provide useful class abstraction capabilities in general.

* supertyping
https://web.archive.org/web/20160508045041/http://repetae.net/recent/out/supertyping.html
Ability to supertype as well as subtype classes. Basically it would allow you too add nodes anywhere in the class inheritance tree rather than just extending the bottom. It is VERY useful to allow reuseability of code in ways the original author did not anticipate.

* HList
https://web.archive.org/web/20061207080942/http://homepages.cwi.nl/~ralf/HList/
Strongly typed heterogeneous collections
- Haskell workshop paper ACM DL
  https://web.archive.org/web/20061207080942/http://doi.acm.org/10.1145/1017472.1017488
- Extended TR: 
  https://web.archive.org/web/20061207080942/http://homepages.cwi.nl/~ralf/HList/paper.pdf
- See also OOHaskell
  https://web.archive.org/web/20061207080942/http://homepages.cwi.nl/~ralf/OOHaskell
- Slides
  https://web.archive.org/web/20061207080942/http://homepages.cwi.nl/~ralf/HList/slides.pdf
- Sources (darcs)
  https://web.archive.org/web/20061207080942/http://darcs.haskell.org/HList/


* TH
https://web.archive.org/web/20051105011641/http://www.haskell.org/th/

* firstify
https://github.com/ndmitchell/firstify
This project relates to a transformation which takes a higher-order program, and a produces an equivalent first-order program. Unlike Reynolds style defunctionalisation, it does not introduce any new data types, and the results are more amenable to subsequent analysis operations. Our transformation is implemented, and works on a Core language to which Haskell programs can be reduced. Our method cannot always succeed in removing all functional values, but in practice is remarkably successful.

* haskell-exchange-2015
https://skillsmatter.com/conferences/7069-haskell-exchange-2015
