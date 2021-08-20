# Basic collections 

collection   | unordered   | ordered
-------------|-------------|--------------------
uniqueness   | { s,e,t }   | < u n i q e s c >
multiplicity | ⟅ b²,a⁴,g ⟆ | ( s,e,q,u,e,n,c,e )


*Sets* do not have the notion of ordered and multiplicity of elements (unordered but do not allow duplicates). These basic properties of a set data structure imply the existence of three additional collections with the remaining permutations of those properties.

*Bags* or *multisets* keep the disorder but relax the uniqueness constraint - a bag has no notion of order and the same element may appear any number of times. Bags have a special feature: being unordered, there's no sense in writing out every single copy of the same element, e.g. `⟅a,b,b,b,c,c⟆`; instead multiple copies of the same element are writen with a superscript denoting their *multiplicity* (i.e. the number of instances) e.g. `⟅a,b³,c²⟆`.

A *sequence* is an ordered collection in which the same element is allowed to appear any number of times. Sequences are the well-known collection regarding only these two constraints, but a sequence itself has further classification on its own, particularly in the dimension of *finitness*

The fourth variant is a collection that is ordered but doesn't allow duplicated elements, but also doesn't have a familiar, or even a name (unique sequence?). However, its modus operandi seemed to be that of a list, only once an element makes its debut, it cannot appear again. This seems like an interesting property that must have good use cases (?).


## List

List (sequences) have several interesting cases that are tested for:
- the empty list
- a singleton list
- a doubleton list
- a tripleton list
- a finite list
- an infinite list


However, there's no way to test whether a list is infinite (or to test for divergence in general). Because of that we have to make such properties explicit in the data type.

So, with a `List ` anything goes, plus finiteness is untestable. To make sure a list is never empty, there is `NonEmpty` variant. A stream is a list guaranteed to be infinite (e.g. by dropping the Nil data ctor). Syntactically, `NonEmpty` and `Stream` look exactly the same, but semantically they have different guaratees: they both expel the empty list, but Stream, unlike NonEmpty, guarantees the sequence is infinite. Without dependent types, there is no way to guarantees a sequence is finite [isthere?].

Stream ⊂ NonEmpty ⊂ List

```hs
data List     a = Nil | Cons a [a]
data NonEmpty a =       Cons a [a]
data Stream   a =       Cons a [a]
```

Lists are linear datastructures (only one path down from the root), thus an infinite list can contain at most one cycle, in which case it can be compacted into a finite cyclic list.
