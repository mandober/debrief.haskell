# Algebraic Structure in mathematics

All axioms of group-like algebras:
* Totality (closure)
* Associativity
* Identity (identity or neutral element)
  - Left Identity
  - Right Identity
  - Total identity (left + right)
* Invertibility (inverse element)
* Commutativity
  - Left Commutativity
  - Right Commutativity
  - Total Commutativity (left + right)

Group-like algebras:
- Magma
- Semigroup
- Monoid
- Group
- Abelian group



In mathematics, an **algebraic structure** (or just **algebra**), is a set, called **carrier set** or the **underlying set**, together with one or more operation that uphold a set of axioms.

**Group-like algebras** are algebraic structure made of a set together with a binary operation that respects closure.

__*Closure*__ or __*totality*__ is a property (an axiom) of the attached operation wrt the elements of a carrier set, that combines two elements to produce a third element that must also be an element of the carrier set.

**Magma** is such an algebra and its binary operation only upholds the property of closure. **Semigroup** is an algebra that besides the axioms of closure, also upholds the axioms of associativity. Semigroup is a Magma but unlike Magma's, Semigroup's binary operation is associative.

__*Associativity*__ is property of the attached operation that can combine any 3 elements of the carrier set in any order. That is, the attached operation has no precedence, so combining `(a and b) and c` is the same as `a and (b and c)`. Due to this the parenthesis are unnecessary so the operation is written without them: `a and b and c`.

**Monoid** is like a Semigroup, but Monoid's binary operation also respects the axiom of identity.

__*Identity*__ is a property of the attached operation and the carrier set - the identity element is such unique element of the carrier set, that when combinated with another element of the carrier set, it leaves that element unchanged. Because of this, the identity element is also called the __*neutral element*__ and it is commonly denoted by `ϵ`. Identity axioms can be further divided into __*left-identity*__ and __*right-identity*__.

**Group** is an algebra with a binary operation over the carrier set that upholds all the mentioned axioms so far, closure, associativity, identity, but it also respects the axioms of invertability.

__*Invertability*__ is a property of the attached operation and the carrier set that, such that each element $$x$$ of the carrier set has its inverse, denoted by $$x^{-1}$$. When the operation combines an element with its inverse it produces the identity element.

**Abelian group** is a group that also upholds the axiom of commutativity.

__*Commutativity*__ is a property of the attached operation and the carrier set that, such that combining elements $$x$$ and $$y$$ produces the same result as combining $$y$$ and $$x$$. That is, the order of combination makes no difference.


All group-like algebra have a single binary operations, here denoted ⨀, and each such algebra upholds at least one from these axioms:

$$\forall x,y,z,ϵ,x^{-1} \in \mathbb{S}$$:
1. Totality: $$\qquad\ \ x ⨀ y \in \mathbb{S}$$
2. Associativity: $$\ \ x ⨀ (y ⨀ z) = (x ⨀ y) ⨀ z = x ⨀ y ⨀ z $$
3. Identity: $$\qquad\ \ x ⨀ ϵ = x = ϵ ⨀ x\quad$$ (left, right, total)
4. Invertibility: $$\quad \ x ⨀ x^{-1} = ϵ = x^{-1} ⨀ x$$
5. Commutativity: $$x ⨀ y = y ⨀ x$$
