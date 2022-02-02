# Use of language extensions - HaskellWiki

> Haskell compilers are full of experimental features.
However, when it comes to designing libraries one should carefully think about which extensions to use and which not.
Every required language extension and every imported library which on its own depends on such an extension,
causes some of the following problems:

Haskell compilers are full of experimental features. However, when it comes to designing libraries one should carefully think about which extensions to use and which not. Every required language extension and every imported library which on its own depends on such an extension, causes some of the following problems:

*   The use of a language extension might indicate a design flaw! Sometimes you write an incorrect program and the compiler suggests to use a language extension to resolve that. It is a quick fix to solve the problem by enabling the extension, but if you do not understand the implications, you run into problems later. See e.g. [List instance](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/List_instance "List instance"), [Overlapping instance](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/index.php?title=Overlapping_instance&action=edit&redlink=1 "Overlapping instance (page does not exist)").
*   It limits the range of Haskell compilers you can use.
*   According to the availability of compilers on different platforms, it will also limit the range of machines it can be run on.
*   It complicates the installation, if a user has to install a particular compiler to use your library.
*   Many extensions are complicated enough to produce even more incomprehensible error messages, than errorneous Haskell 98 programs.
*   Even with the same compiler, in a year or two your code might need fixing if the extension or implementation is sufficiently experimental (for example, the rules for what type-signatures GADTs require, has changed from GHC 6.4 to 6.6 to 6.8 to 6.10). People who depend on your library may be impatient to upgrade to the newest version of GHC.

We suggest the following hierarchy of complexity with respect to types:

*   Simple algebraic types
*   H98 type classes
*   Fancier instance heads (but still single-parameter, non-overlapping)
*   [Existential](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Existential_type "Existential type") and local universal quantification
*   [GADTs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/GADT "GADT") (I'm still uncertain about this judgement, but they seem to be less troublesome than...)
*   Multiparameter type classes with [functional dependencies](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Functional_dependencies "Functional dependencies")
*   [Multi-parameter type classes](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Multi-parameter_type_classes "Multi-parameter type classes") including [undecidable instances](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/index.php?title=Undecidable_instance&action=edit&redlink=1 "Undecidable instance (page does not exist)"), [overlapping instances](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/index.php?title=Overlapping_instance&action=edit&redlink=1 "Overlapping instance (page does not exist)")
*   [Template Haskell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Template_Haskell "Template Haskell")

We recommend to explicitly switch on language extensions that are needed using the LANGUAGE pragma instead of switching them on all at once using the `-fglasgow-exts` compiler flag.

See also
--------

*   Initiating mail on Haskell Cafe: [When to use fancy types](http://www.haskell.org/pipermail/haskell-cafe/2005-May/010085.html)
*   GHC ticket on [warn about language extensions that are not used](http://hackage.haskell.org/trac/ghc/ticket/3085)
*   Haskell Cafe on [List and description of language extensions](http://www.haskell.org/pipermail/haskell-cafe/2009-April/059206.html)


[Source](https://wiki.haskell.org/Use_of_language_extensions)