# Type-level programming

- type ctors
- data ctors
- promoted data types
- kind-level functions
- term-level functions
- term-level functions
- GADTs
- phantom types
- Proxy
- singletons
- type classes
- inductive classes, recursive classes
- functional dependencies
- type families
- data families


The techniques used:
- phantom types
  [Leijen and Meijer 1999]
- nested datatypes
  [Bird and Paterson 1999; Okasaki 1999]
- higher-order polymorphism encoding of Leibniz equality
  [Baars and Swierstra 2002; Cheney and Hinze 2002; Weirich 2004]
- overlapping type classes
  [Kiselyov et al. 2004]
- tagless algebra
  [Carette et al. 2009]
- functional dependencies
  [Guillemette and Monnier 2008a; McBride 2002]


* 1999 `de Bruijn notation as a nested datatype` R. S. Bird, R. Paterson
* 2002 `Typing dynamic typing` A. I. Baars, S. D. Swierstra
* 2002 `A lightweight implementation of generics and dynamics` J. Cheney, R. Hinze
* 2009 `Finally tagless, partially evaluated: Tagless staged interpreters for simpler typed languages` J. Carette, O. Kiselyov, C. Shan 
* 2010 `Parametricity and dependent types` J.P. Bernardy, P.Jansson, R.Paterson
* 2012 `Step-indexed normalization for a language with general recursion` C. Casinghino, V. Sjoberg, S. Weirich
