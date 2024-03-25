# Bird-Meertens formalism

https://en.wikipedia.org/wiki/Bird-Meertens_formalism

**The Bird-Meertens formalism (BMF)**, aka Squiggol, is a calculus for deriving programs from specifications in a FP setting using equational reasoning.

## Basic examples and notations

Map is a well-known second-order function that applies a given function to every element of a list. In BMF, it is denoted by a `*`

    f * [a, b, …, z] = [f a, f b, …, f z]


Fold (reduce) is a function that summarizes a list by a repeated application of a binary operator `⊕`. It is written `/` in BMF. Taking `⊕` as a suitable binary operator with neutral element `e`, we get

    ⊕ / [a, b, …, z] = e ⊕ a ⊕ b ⊕ … ⊕ z

Using those two operators, and the primitives (+) for addition and (⫲) for list concatenation, we can express several familiar functions in point-free style:
- summing a list: `sum = +/`
- flattening a list: `flatten = ⫲/`

Using `∘` for composition and `∧` for conjunction, we can implement the `filter` function as `all p = (∧/) ∘ (p*)`.

```
all p [a, b, c]
= (∧/) ∘ (p∗) [a, b, c]
= ∧/ (p∗ [a, b, c])
= ∧/ [p a, p b, p c]
= p a ∧ p b ∧ p c
```


Bird (1989) transforms inefficient easy-to-understand expressions ("specifications") into efficient involved expressions ("programs") by algebraic manipulation.

For example, the specification `max ∘ mapsum ∘ segs` is an almost literal translation of the maximum segment sum problem, but running that functional program on a list of size `n` will take time O(n3) in general. From this, Bird computes an equivalent functional program that runs in time O(n), and is in fact a functional version of *Kadane's algorithm*.

    The derivation is shown in the picture, with computational complexities given in blue, and law applications indicated in red. Example instances of the laws can be opened by clicking on [show]; they use lists of integer numbers, addition, minus, and multiplication. In the example instances, lists are colored by nesting depth; in some cases, new operations are defined ad hoc (grey boxes).

The notation in Bird's paper differs from that used above: `map`, `concat`, and `foldl` correspond to `∗`, `flatten`, and a generalized version of `/` above, respectively, while `inits` and `tails` compute a list of all prefixes and suffixes of its arguments, respectively. Function composition is denoted by `∘`, which has lowest binding precedence.


The maximum segment sum problem

```hs
  max . map sum  . segs
= max . map sum  . concat        . map tails . inits
= max . concat   . map (map sum) . map tails . inits
= max . map max  . map (map sum) . map tails . inits
= max . map (max . map sum . tails)        . inits
= max . map (foldl (⨂) 0)                . inits
= max . scanl (⨂) 0
= fst . foldl (⨀) (0, 0)

x ⨂ y = (x + y) ↑ 0
(u,v) ⨀ x = let w = (v + x) ↑ 0 in (u ↑ w, w)

segs ≡ concat . map tails . inits         -- segs def

map sum . concat ≡ concat . map (map sum) -- map promotion

max . concat ≡ max . map max              -- max def

map max . map (map sum) . map tails
 ≡ map (max . map sum . tails)            -- map distro

max . map sum . tails ≡ foldl (+) 0       -- Horner's rule

map (foldl (+) 0) . inits ≡ max . scanl (+) 0
```


## Homomorphism lemma

The homomorphism lemma and its applications to parallel implementations.

A function `h` on lists is called a *list homomorphism* if there exists an associative binary operator `⊕` and neutral element `e` such that this holds:

```hs
h []       = e
h (l ++ m) = h l ⊕ h m
```

The **homomorphism lemma** states that `h` is a homomorphism iff there exists an operator `⊕` and a function `f` such that `h = (⊕/) ∘ (f*)`.

A point of great interest for this lemma is its application to the derivation of highly parallel implementations of computations. Indeed, it is trivial to see that `f*` has a highly parallel implementation, and so does `⊕/`, most obviously as a binary tree.

Thus, for any list homomorphism `h`, there exists a parallel implementation. That implementation cuts the list into chunks, which are assigned to different computers; each computes the result on its own chunk. It is those results that transit on the network and are finally combined into one.

In any application where the list is enormous and the result is a very simple type (like an integer), the benefits of parallelisation are considerable. This is the basis of the *map-reduce* approach.


## History

BMF was devised by Richard Bird and Lambert Meertens as part of their work within IFIP Working Group 2.1. It is sometimes referred to in publications as BMF, as a nod to Backus-Naur form. Facetiously, it is also referred to as Squiggol, as a nod to ALGOL, which was also in the remit of WG 2.1, and because of the "squiggly" symbols it uses. A less-used variant name, but actually the first one suggested, was *SQUIGOL*. Martin and Nipkow provided automated support for Squiggol development proofs, using the Larch Prover.

`Automating Squiggol` - Ursula Martin, Tobias Nipkow, 1990
(In Manfred Broy; Cliff B. Jones (eds.). Proc. IFIP WG 2.2/2.3 "Working Conference on Programming Concepts and Methods", North-Holland)
https://www21.in.tum.de/~nipkow/pubs/squiggol.html

## Refs

https://en.wikipedia.org/wiki/Bird-Meertens_formalism
https://en.wikipedia.org/wiki/Larch_Prover
https://en.wikipedia.org/wiki/Larch_family

* The School of Squiggol: A History of the Bird−Meertens Formalism
- by Jeremy Gibbons
https://www.cs.ox.ac.uk/publications/publication13852-abstract.html
https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/squiggol-history.pdf
Abstract: The Bird-Meertens Formalism, colloquially known as "Squiggol", is a calculus for program transformation by equational reasoning in a function style, developed by Richard Bird and Lambert Meertens and other members of IFIP Working Group 2.1 for about two decades from the mid 1970s. One particular characteristic of the development of the Formalism is fluctuating emphasis on novel 'squiggly' notation: sometimes favouring notational exploration in the quest for conciseness and precision, and sometimes reverting to simpler and more rigid notational conventions in the interests of accessibility. This paper explores that historical ebb and flow.


* `Larch: Languages and Tools for Formal Specification`, John V. Guttag, James Horning, 1993 (with S. J. Garland, K. D. Jones, A. Modet, and J. M. Wing). Springer-Verlag Texts and Monographs in Computer Science
https://www.cs.cmu.edu/afs/cs/usr/wing/www/publications/LarchBook.pdf

Larch
http://www.sds.lcs.mit.edu/spd/larch/

LP, the Larch Prover -- Introduction
http://www.sds.lcs.mit.edu/spd/larch/LP/overview.html
