# Relations

Relations
- `=`     syntactic equality,   M = M
- `≡ᵦ`    β-equivalence,        M ≡ᵦ Mʹ
- `=ᵦ`    β-convertibility,     M =ᵦ Mʹ
- `=ᵦ=`   β-convertibility,     M =ᵦ= Mʹ
- `=ᵅ`    α-equivalence,        M =ᵅ Mʹ
- `=α=`   α-equivalence,        M =α= Mʹ
- `=η=`   η-equivalence,        F =η= λx.Fx
- `⟶ᵦ`   small-step semantics, M ⟶ᵦ Mʹ
- `-->ᵦ`  small-step semantics, M -->ᵦ Mʹ
- `-->ᵦ⋆` big-step semantics,   M -->ᵦ⋆ Mʹ
- `-->>ᵦ` big-step semantics,   M -->>ᵦ Mʹ

Equivalence symbols
- α-equivalence, `=α=`, `=ᵅ`
- β-equivalence, `=β=`, `≡ᵦ`
- η-equivalence, `=η=`

Conversions
- β-reduction, `-->ᵦ`, `-->ᵦ⋆`, 
  (λx.x)(λy.y) `⟶ᵦ` [x:=λy.y]x `=` λy.y
  (λx.x)(λy.y) `⟶ᵦ` [x:=λy.y]x `≡ᵦ` λy.y
- α-conversion, `λx.E =α= [x:=xʹ]E`, e.g. λx.x =α= λxʹ.xʹ
- η-conversion
  - η-reduction, e.g. `F =η= λx.Fx`
  - η-expansion, e.g. `λx.Fx =η= F`
- β-convertibility equivalence of terms: `M =ᵦ= Mʹ`
- Single-step evaluiation relation: `M ⟶ᵦ Mʹ`
- Multi-step evaluation relation: `M ->>ᵦ Mʹ`
- Alpha-equivalence relation: `M =α= Mʹ`

## Equality and equivalence

- Syntactic equality, `M = M`
  - λx.λy.x =  λx.λy.x
  - λx.λy.x ≡ᵦ λx.λy.x
- ?-equivalence modulo renaming
  - λx.λy.x =α= λa.λb.a
- β-convertibility
  - (λx.λy.x)(λa.a)(λb.b) =ᵦ λb.b (goes both ways)
- single-step β-reduction
  - (λx.λy.x)(λa.a)(λb.b) ⟶ᵦ (λy.(λa.a))(λb.b) ⟶ᵦ λb.b
- multi-step β-reduction
  - (λx.λy.x)(λa.a)(λb.b) ->>ᵦ λb.b
