# Abstract algebra

Abstract algebra is a part of algebra that studies algebraic structures, also called algebras.

**Algebraic structure** is a mathematical structure consisting of a set, called the *carrier set*, which is endowed with one or more *binary operations* and a set of *axioms* that it upholds.

The simplest algebraic structures are group-like: a carrier set is endowed with a single associative binary operation that is closed over the carrier set.

**Closore** or **totality** is a fundamental axiom that all algebras respect: If `a` and `b` are elements of the carrier set `A`, then the closure demands that the combination of `a` and `b` under the binary operation `⨀` is also an element of set A: `a, b ∈ A ⇒ a ⨀ b ∈ A`. All algebras uphold the closure axiom, and since it is always assumed, it is rarely mentioned explicitly.

**Magma** is an algebraic structure that is an embodiment of the *closure*. Since closure is the only required axiom for an algebra to be considered a magma, most algebraic structures are magmas. However, when mathematicians mention a magma they usually mean an algebra that obeys closure but also *does not obey associativity and unit axioms*. Even though formally, a monoid is an algebra that obeys all three mentioned axioms (closure, assoc, unit), which implies that all monoids are magmas. Most of the time, a monoid means an algebra with closure, assoc, unit, but without invertability (even though al lgroups are monoids).

**Semigroup** is an algebraic structure that is an embodiment of the axiom of *associativity*. As already mentioned, the axiom of closure is assumed, so a semigroup is a magma whose binop is associative.

**Monoid** is an algebraic structure that is an embodiment of the axiom of *identity*. The axiom of closure is assumed, so a monoid is a semigroup with an identity element.

**Group**  is an algebraic structure that is an embodiment of the axiom of *invertability*. The axiom of closure is assumed, so a group is a monoid that has invertable elements.

Basic axioms
- [Closure]:       a,b ∈ A.    a ⨀ b ∈ A
- [Assoc]:         a,b,c ∈ A.  a ⨀ (b ⨀ c) = (a ⨀ b) ⨀ c
- [UnitL]:         a,ϵ ∈ A.    ϵ ⨀ a = a
- [UnitR]:         a,ϵ ∈ A.    a ⨀ ϵ = a
- [InvertL]:       a,a̅,ϵ ∈ A.  a̅ ⨀ a = ϵ
- [InvertR]:       a,a̅,ϵ ∈ A.  a ⨀ a̅ = ϵ

Additional axioms
- [Commutativity]: a,b ∈ A.  a ⨀ b = b ⨀ a
- [Idempotence]:   a ∈ A.    a ⨀ a = a
- [Domination]:    a,ζ ∈ A.  a ⨀ ζ = ζ

Algebraic structures with 1 binop
- `A` = carrier (underlying) set
- `⨀` = binary operation closed over the carrier set
- `ϵ` = unit, identity or neutral element (sometimes denoted by `1`)
- `ζ` = counit, cacellative element, zero element (sometimes denoted by `0`)

Given a carrier set `A` as the underlying set of some monodial algebra, there is a *single* *unique* element in this set, called **unit** or identity element or neutral element, denoted by `ϵ` (and sometimes by `1`). The main property of the unit element is that it is neutral regarding the binop. The result of the binop when one of the operands is `ϵ` is always the other operand.

This implies the unit axiom can be split into 2 axioms: one axiom for the left unit, [UnitL] `ϵ ⨀ a = a`, when `ϵ` is the left operand, and one axiom for the right unit, [UnitR] `a ⨀ ϵ = a`, when `ϵ` is the right operand. When both UnitL and UnitR are upheld, we just say that the algebaic structure repects the [Unit] axiom, `a ⨀ ϵ = a = ϵ ⨀ a`. Also note that `ϵ ⨀ ϵ = ϵ`.

>Moreover, there can only ever be a **single unique unit** per carrier set.

Let `ϵ, η ∈ A` be unit elements, then
```
ϵ ⨀ a = a
η ⨀ a = a
ϵ ⨀ a = η ⨀ a   Removing 'a' from both sides
ϵ = η
```


Monoid ⟨A, ⨀, ϵ⟩
- [closure] a,b ∈ A ⇒ (a ⨀ b ∈ A)
- [assoc]   a ⨀ (b ⨀ c) = (a ⨀ b) ⨀ c
- [unitL]   ϵ ⨀ a = a
- [unitR]   a ⨀ ϵ = a
  where
  - `A` = carrier set
  - `⨀` = binop
  - `ϵ` = identity element

Group-like algebras
- `⟨A, ⨀⟩`        = magma
- `⟨A, ⨀⟩`        = semigroup
- `⟨A, ⨀, ϵ⟩`     = monoid
- `⟨A, ⨀, ϵ, ⁻¹⟩` = group



Since closure is the only required axiom for an algebra to be considered a magma, most algebraic structures are magmas.
