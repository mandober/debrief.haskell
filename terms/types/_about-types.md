# About types



The Curry-Howard correspondence reveals relation between logic, set theory and programming. What is of interest for us here are these correspondence:

* A set is a collection of objects. However, a set is also an object in its own right, so a set may contain other sets; a set cannot contain itself.
* An object is called an element when it is a member of a set; all objects *are* elements because there is no set-less object floating around, all alone and outside of some set (as far as we're concerned, here; actively ignoring the set of all sets and its membership clubs).
* However, not all sets have elements, viz. the empty set has none (it is the only set needed to exist to make a set theory work). The empty set will often be the identity element when push comes to shove.


* A type corresponds to a set. A type is a collection of values.

* The empty set corresponds to the empty type. The empty set is unique (hence the "the"), that is, there is only one empty set (no aliasing).

has none (and it is the only set needed to exist to make a set theory work).


It is an inhabited type.
* A type is inhabited if it has at least one member.


A value of some particular type may be called a typed value; all values belong to some type, there are no typeless values floating around outside the type constraints.

* Objects often belong to more than one set, which is not the case with values.



belong to some set, at least for our purposes here, so all object are elements).

which are called elements (when they are )


- set ~ type
- (set) object ~ value (of some type)
- object (set element) ~ term (value of a type)
- domain set ~ input type
- codomain set ~ output type

A function is a mapping that associates an element (term) in a domain set (input type) to an element (term) in a codomain set (output type).
