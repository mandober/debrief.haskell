# About mathematical functions

* Functions are mappings that associate an element (term) in a domain set (input type) to an element (term) in a codomain set (output type).

* Due to currying all functions may be presented as unary functions, taking one input type to one output type.

* A function `f: A ↦ B` is a **set of ordered pairs**, `f = { (a, b) }`   
such that `a ∈ A` and `b ∈ B`, i.e. the first component of the ordered pair is always an element from the domain, and the second component of the ordered pair is always an element from the codomain. So a function is a set of ordered pairs of input and output elements, i.e. pairs made of the input value and the output (resulting) value, `f = { (a, f(a)) = b }`.

* **All domain elements must be associated** to a codomain element; that is, each element in the domain must be involved in this functional relation (all functions are relations).

> ∀a ∈ A. !∃b ∈ B. f: A ↦ B ∧ f(a) = b

`!∃b ∈ B` means that for each `a` in `A` *there exists a unique element* `b` in `B` such that `f(a) = b`.

* Each domain element must be associated **to exactly one codomain element**.   
If  `a` is an element from the domain set `A`, `a ∈ A`    
and `b` is an element from the codomain set `B`, `b ∈ B`   
and `f` is a function from `A` to `B`, `f: A ↦ B`     
such that `f(a) = b`    
and if `x` is an element from the codomain set `B`, `x ∈ B`   
then `f(a) = x` if and only if `b = x`.

* Concerning the codomain, more than one element may be in association with the same domain element. This means, if `f(a)=a*a` then `(0,1)` and `(1,1)` are among the ordered pairs that constitute this function, even though both domain elements (0 and 1) are associated with the same codomain element (1).

* It is not the case that all the elements of the codomain must be involved in the functional relation. The subset of elements that are is called a **range**. Sometimes, this makes determining the exact codomain somewhat of a problem. Knowing that a relation is a function, we know that the domain consists of all the elements that are the first component in the ordered pairs of the set of a function (that is, we know that all the entire domain must be involved; the elements that are involved in a function are the domain). This is not the case with codomain for the range is not equal to the codomain (if codomain = range then the function is *surjective*). This means there are many functions of the same form that differ only in the size of codomain! If the range is some defined subset there are infinitely many supersets containing this subset (containing the range).

> To be absolutely precise when defining a function, one must also explicitly specify the exact codomain.

Since a function may be defined by specifying an arbitrary mapping, e.g.

```
a -> w
b -> y
c -> y
d -> z
```

This leaves the determination of the codomain ambiguous. We cannot possibly know whether it is equal to the range = {w,y,z}, or larger; and if larger, we cannot even presume what the additional element making up the codomain could be.

* Considering a direction, a function `f: A ↦ B` is obviously directed left to right, i.e. from domain to codomain (in fact, all functions *are*). The tail of an arrow is at each element of the domain and each arrow ends at exactly one element in the codomain. If each arrow would have a unique head element, such function would be called **injective**. This means that there is a 1 to 1 mapping alright, but not all of the codomain is involved. If all of the codomain is involved (range = codomain) then a function is **surjective** (still, some codomain elements arein association with more than one domain element). If a function is both injective and surjective, then it is a **bijective** function; it is total 1 to 1 mapping (domain = codomain = range).

* Bijective functions are fully invertable being a 1:1 mapings.

* Injective functions are partially invertable. They don't squish the image, but there are elements in the codomain (which becomes the domain of the inverted function) for which the inversion is undefined. Inverting a total injective function makes a partial injective function, which may be considered an injective (or bijective even) partial function.

* Surjective functions "squish" the image, loosing information. Looking from the opposite (right to left) direction, it is a "many to one" association.

> Bijective functions are invertable. Injective function is partially invertable. When inverted, a surjective function is demoted to a relation.

- total     : dom > ran, ran ⊆ cod, dom ? cod
- surjective: dom >= ran, `ran = cod`, dom >= cod
- injective : `dom = ran`, ran ⊆ cod, dom ? cod
- bijective : dom > ran, ran ⊆ cod, dom ? cod

* By the definition of functions itself, all elements in the domain must be associated, therefore functions are **total**. The concept of a **partial function** is a relaxation of this requirement. In math, the division function over the set of real numbers is partial because it is not defined if the divisor is 0.
