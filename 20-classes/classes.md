# Type classes


## Algebraic Structure in mathematics

In mathematics, an **algebraic structure** (or just **algebra**), is a set, called **carrier set** or the **underlying set**, together with one or more operation that uphold a set of axioms.

**Group-like algebras** are algebraic structure made of a set together with a binary operation that respects closure.

*Closure* or *totality* is a property (an axiom) of the attached operation wrt the elements of a carrier set, that combines two elements to produce a third element that must also be an element of the carrier set.

**Magma** is such an algebra and its binary operation only upholds the property of closure. **Semigroup** is an algebra that besides the axioms of closure, also upholds the axioms of associativity. Semigroup is a Magma but unlike Magma's, Semigroup's binary operation is associative.

*Associativity* is property of the attached operation that can combine any 3 elements of the carrier set in any order. That is, the attached operation has no precedence, so combining `(a and b) and c` is the same as `a and (b and c)`. Due to this the parenthesis are unnecessary so the operation is written without them: `a and b and c`.

**Monoid** is like a Semigroup, but Monoid's binary operation also respects the axiom of identity.

*Identity* is a property of the attached operation and the carrier set - the identity element is such unique element of the carrier set, that when combinated with another element of the carrier set, it leaves that element unchanged. Because of this, the identity element is also called the *neutral element* and it is commonly denoted by `ϵ`. Identity axioms can be further divided into *left-identity* and *right-identity*.

**Group** is an algebra with a binary operation over the carrier set that upholds all the mentioned axioms so far, closure, associativity, identity, but it also respects the axioms of invertability.

*Invertability* is a property of the attached operation and the carrier set that, such that each element $$x$$ of the carrier set has its inverse, denoted by $$x^{-1}$$. When the operation combines an element with its inverse it produces the identity element.


All group-like algebra have a single binary operations, here denoted ⨀, and each such algebra upholds at least one from these axioms:

$$\forall x,y,z,ϵ,x^{-1} \in \mathbb{S}$$:
1. Totality: $$\qquad\ \ x ⨀ y \in \mathbb{S}$$
2. Associativity: $$\ \ x ⨀ (y ⨀ z) = (x ⨀ y) ⨀ z = x ⨀ y ⨀ z $$
3. Identity: $$\qquad\ \ x ⨀ ϵ = x = ϵ ⨀ x$$
4. Invertibility: $$\quad \ x ⨀ x^{-1} = x^{-1} ⨀ x = ϵ$$
5. Commutativity: $$x ⨀ y = y ⨀ x$$


## Type classes in Haskell

Haskell takes inspiration from math, only sets are replaced with types, and set members are values that inhabit a type. The attached operations remain similar and each corresponsing operation obeys the same set of axioms.

Haskell implements these algebras as type classes. A **type class** defines a set of operations (functions, methods) that a type wanting to implement such type class must define.

For example, since there are many types of numbers in Haskell and since it would be reasinable that the plus operator could be used with each of them, instead of each number type defining its own addition and other common numeric operations, the type class declares these operations instead. Then when a number type defines its own implementation of the needed operation, we benefit by using the same symbol for e.g. addition no matter which types we want to add, as long as they are *instances* of the appropriate class. 

An **instance** of a class is a type that defines its own implementation of operations a type class declares.

In fact each class defines the minimal set of operation a type must define, but offers many more that can be derived from from that minimal set. However, each type is free to override the methods these default methods it gets "for free" with a possibly more efficient implementation.

In Haskell, we think of types as having an instance of a typeclass. When we represent abstract operations that can be reused across a set of types, we usually represent them as a typeclass. Type classes give us a way to recognize, organize, and use common functionalities and patterns across types that differ in one but are similar in another way.
