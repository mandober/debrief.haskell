# Rules

Rules
- β-reduction, `-->ᵦ`, `-->ᵦ⋆`, e.g. (λx.x)(λy.y) -->ᵦ [x:=λy.y]x ≡ᵦ λy.y
- α-conversion, `λx.E =α= [x:=xʹ]E`, e.g. λx.x =α= λxʹ.xʹ
- η-conversion
  - η-reduction, e.g. `F =η= λx.Fx`
  - η-expansion, e.g. `λx.Fx =η= F`
