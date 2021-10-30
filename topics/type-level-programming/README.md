# Type level programming

Type level programming
  - data type construction
    - ADT
      - the empty type,    `0`,    `Void`
      - unit type,         `1`,    `()`
      - sum types,         `+`,    `a + b`
      - product types,     `⨯`,    `a ⨯ b`
      - exponential types, `→`,    `a -> b` ≅ `bᵃ`
      - inductive types,   `μ`,    `μa = 1 + a ⨯ μa`
    - GADT
  - data types
    - type ctors
      - first-order type ctors
      - higher-order type ctors
  - kinds
    - first-order
    - higher-order
  - data types promotion
  - type classes
    - inductive classes
    - type class meta-programming
  - type families
  - data families

  * Rank polymorphism
    - Rank-1 polymorphism
      - prenex polymorphism
      - let-polymorphism
    - Rank-2 polymorphism
    - Rank-k polymorphism
    - Rank-n polymorphism
    - Rank-n polymorphism
      - Higher-rank polymorphism
      - Arbitrary-rank polymorphism

  * Kind polymorphism
    - Higher-kinded polymorphism
      - Higher-kinded type
    - Higher-ranked polymorphism
      - Higher-ranked type
