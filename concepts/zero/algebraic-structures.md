# Algebraic structures

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



In math, an **algebraic structure**, or **algebra**, is a set, called the **carrier set** or the underlying set, along with, at least one, *binary operation*, and a set of *axioms*, that the carrier set and operations together, must uphold.

**Group-like algebras** are algebraic structure comprised of a carrier set with a binary operation over the carrier set and the axiom of closure as the most basic one.

**Magma** is the most basic group-like algebra for its binary operation only satisfies the axiom of closure.

**Semigroup** is a magma + associativity; semigroup's binary operation is associative over the carrier set.

**Monoid** is a semigroup + identity.

**Group** is a monoid + invertability.

**Abelian group** is a group + commutativity.

All group-like algebra have 1 binary operation (here denoted ⨀). 
This is the list of all group-like axioms:
1. Closure:       ∀x ∀y ∈ S   ->   x ⨀ y ∈ S
2. Associativity: ∀x ∀y ∀z ∈ S.    x ⨀ (y ⨀ z) = (x ⨀ y) ⨀ z = x ⨀ y ⨀ z
3. Identity:      ∀x !∃ϵ ∈ S.      x ⨀ ϵ = x = ϵ ⨀ x
4. Invertibility: ∀x ∃x⁻¹ !∃ϵ ∈ S. x ⨀ x⁻¹ = ϵ = x⁻¹ ⨀ x
5. Commutativity: ∀x ∀y ∈ S.       x ⨀ y = y ⨀ x


**Closure** or **totality** is the axiom related to the results of operation when two elements of the carrier set are combined with the binary operation. 
`∀x ∀y ∈ S -> x ⨀ y ∈ S` that is, if x and y are elements of the carrier set S then so is the result of combining them using the attached binary operation. In such cases we say that the binary operation is closed over the set (the operation has the closure over the set). For example, the set ℕ is closed under addition but not under subtraction. In PLs, types correspond to sets, and operations attached to a carrier set correspond to functions (methods).
