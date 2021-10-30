# Polymorphic Recursion

https://en.wikipedia.org/wiki/Polymorphic_recursion

**Polymorphic recursion**, also called *Milner-Mycroft typability* or *Milner-Mycroft calculus*, refers to a *recursive parametrically polymorphic function* where the type parameter changes with each recursive invocation made, instead of staying constant.

Type inference for polymorphic recursion is equivalent to semi-unification and therefore *undecidable* and requires the use of a semi-algorithm or programmer supplied type annotations.

## Nested datatypes

A **nested datatype**, also known as a **non-regular datatype**, is a parametrised datatype whose declaration involves different instances of the accompanying type parameters.

Consider the following nested datatype:

```hs
data Set a = a :. (Set [a]) | Eset

data Nested a = a :. (Nested [a]) | Epsilon
infixr 5 :.

nested = 1 :. [2,3,4] :. [[5,6],[7],[8,9]] :. Epsilon
```

The `length` function defined over this datatype will be polymorphically recursive, as the type of the argument changes from `Nested a` to `Nested [a]` in the recursive call:

```hs
length :: Nested a -> Int
length Epsilon    = 0
length (_ :. xs) = 1 + length xs
```


## Higher-ranked types

(see *Higher-ranked types*)


## Applications

### Program analysis

In *type-based program analysis*, polymorphic recursion is often essential in gaining high precision of the analysis.

Notable examples of systems employing polymorphic recursion include *Dussart, Henglein and Mossin's binding-time analysis* and the *Tofte-Talpin region-based memory management system*.

As these systems assume the expressions have already been typed in an underlying type system (not necessary employing polymorphic recursion), inference can be made decidable again.

### Data structures, error detection, graph solutions

Data structures in FP often use polymorphic recursion to simplify type error checks and solve problems with the intermediary temporary data structures whose proliferation tends to consume a lot of resources, especially memory. This is particularly the problem wrt the more traditional data structures, such as trees, where tree *fusion* is a common antidot.

In his book "Purely Functional Data Structures" from 1999, Chris Okasaki gives a `Cons` example in Haskell wherein the polymorphic type system automatically flags programmer errors. The recursive aspect is that the type definition assures that the outermost constructor has a single element, the second a pair, the third a pair of pairs, etc. recursively, setting an automatic error finding pattern in the data type.



## References

* {{cite journal|first=Lambert|last=Meertens|author-link=Lambert Meertens|title=Incremental polymorphic type checking in B|journal=ACM Symposium on Principles of Programming Languages (POPL), Austin, Texas|year=1983|url=http://www.kestrel.edu/home/people/meertens/publications/papers/Incremental_polymorphic_type_checking_in_B.pdf}}

* `Polymorphic type schemes and recursive definitions` Alan Mycroft, 1984
International Symposium on Programming, Toulouse, France, volume 167
series Lecture Notes in Computer Science
doi: 10.1007/3-540-12925-1_41
ISBN: 978-3-540-12925-7

* `Type inference with polymorphic recursion` Fritz Henglein, 1993
journal: ACM Transactions on Programming Languages and Systems vol15
doi: 10.1145/169701.169692

* `Polymorphic type inference` Michael I. Schwartzbach, 1995
Technical Report BRICS-LS-95-3
http://www.brics.dk/LS/95/3/BRICS-LS-95-3.ps.gz

* `Type reconstruction in the presence of polymorphic recursion`
Assaf Kfoury, J. Tiuryn, P. Urzyczyn, 1993
ACM Transactions on Programming Languages and Systems, vol15
doi: 10.1145/169701.169687

* `Nested Datatypes` - Richard Bird, Lambert Meertens, 1998
http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.3551

* `Extending the type checker for SML by polymorphic recursion: A correctness proof` - Martin Emms,Hans Leiß, 1996
http://www.cis.uni-muenchen.de/~leiss/polyrec/polyrec.cisbericht.96-101.ps.gz

* `Practical Type Inference for Polymorphic Recursion: an Implementation in Haskell` - C. Vasconcellos, L. Figueiredo, C. Camarao, 2003
https://www.academia.edu/download/34646914/Vasconcellos_C.pdf

* `Type Inference for Polymorphic Recursive Definitions: a Specification in Haskell` - L. Figueiredo, C. Camarao
http://www.dcc.ufmg.br/~camarao/ml0-impl.ps

* `Programming Examples Needing Polymorphic Recursion`
J. Hallett, A. J. Kfoury, 2005
doi: 10.1016/j.entcs.2005.06.014
