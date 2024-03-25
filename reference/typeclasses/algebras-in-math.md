# Algebras in math


Group-like algebras   |Tot|Ass|Uni|Inv|COM|IDE|
----------------------|---|---|---|---|---|---|
Magma                 | 1 | 0 | . | . | . | . |
Semigroup             | 1 | 1 | 0 | . | . | . |
Monoid                | 1 | 1 | 1 | 0 | . | . |
Group                 | 1 | 1 | 1 | 1 | 0 | . |

Idempotent group-like |Tot|Ass|Uni|Inv|COM|IDE|
----------------------|---|---|---|---|---|---|
Idempotent Magma      | 1 | 0 | . | . | . | 1 |
Idempotent Semigroup  | 1 | 1 | 0 | . | . | 1 |
Idempotent Monoid     | 1 | 1 | 1 | 0 | . | 1 |
Idempotent Group      | 1 | 1 | 1 | 1 | 0 | 1 |


Commutative group-like|Tot|Ass|Uni|Inv|COM|IDE|
----------------------|---|---|---|---|---|---|
Commutative Magma     | 1 | 0 | . | . | 1 | . |
Commutative Semigroup | 1 | 1 | 0 | . | 1 | . |
Commutative Monoid    | 1 | 1 | 1 | 0 | 1 | . |
Commutative Group     | 1 | 1 | 1 | 1 | 0 | . |


algebras              |Tot|Ass|Uni|Inv|COM|IDE|
----------------------|---|---|---|---|---|---|
Groupoid              | 1 | . | . | . | . | . |
Setoid                | 1 | . | . | . | . | . |

Magmas
- associative magma (nonsense)
- unital magma
- invertible magma
- commutative magma
- idempotent magma

Semigroups
- unital semigruop

Monoids
- invertable monoid

Groups
- Commutative Group (semi-nonsense) (say: Abelian group)
- idempotent group





ALGEBRAS              |ops|ao|az|ai|mo|mz|mi|
----------------------|---|--|--|--|--|--|--|
Ring                  | 2 | +| 0| ⁻| ×| 1|⁻¹|
Semiring              | 2 | +| 0| ⁻| ×| 1|⁻¹|


- 1 binary op, ⨀
  - unit, zero, identity element, ϵ, ϵ ⨀ a = a = a ⨀ ϵ
    - left unit,  ϵ ⨀ a = a
    - right unit, a ⨀ ϵ = a
  - inverse, inverse elements, inverse op, (⁻¹), a⁻¹ ⨀ a = ϵ = a ⨀ a⁻¹
    - left inverse,  a⁻¹ ⨀ a = ϵ
    - right inverse, a ⨀ a⁻¹ = ϵ
- 2 binary ops (closure always)
  - additive, ⨁
    - additive zero, 0
    - additive inverse, (⁻¹)
  - multiplicative, ⨂
    - multiplicative zero, 1
    - multiplicative inverse, (⁻¹)



- totality, closure
- associativity
- unit, identity
- inverse
+ commutativity
+ idempotence
+ distributivity (2 ops)



## Axioms of algebras

S  CS  Carrier Set (type), `S`, `𝒮`, `𝕊`

L  CL  Closure
A  AS  Associativity
I  ID  Identity
N  IN  Inverse
C  CO  Commutativity

M  IP  Idempotency
C  CA  Cancellativity

D  DO  Dominance
A  AN  Annihilation
D  DI  Distributivity

<>   BO  Binary operation, ⨀ (combinins two elements of the carrier set)

0   AZ  Additive zero
+   AO  Additive binary operation, ⨁
-   AI  Additive inverse

1   MZ  Multiplicative zero
×   MO  Multiplicative binary operation, ⨂
⁻¹  MI  Multiplicative inverse


## Examples of algebras
- (𝔹, ∧, True)
- (𝔹, ∨, False)
- (𝔹, ⊕, False)
- (ℕ, +, 1)
- (ℕ, ×, 0)
