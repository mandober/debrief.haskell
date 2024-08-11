# Substitution

1.      x[x:=N] ≡ N                   since x = x
1.      y[x:=N] ≡ y                      if x ≠ y
2.  (P Q)[x:=N] ≡ (P[x:=N]) (Q[x:=N])
3. (λy.P)[x:=N] ≡ λz.(P[y⟼z][x:=N])  if `λz.P[y⟼z]` is an α-variant    
                                      of `λy.P` such that `z ∉ FV(N)`
