## Rules of inference


```
a   b               a ∧ b        a ∧ b
----- ∧I            ----- ∧E₁    ----- ∧E₂
a ∧ b                 a            b


A   B
----- ∧I
A ∧ B


A & B
------ [&E1]
  A

A & B
------ [&E2]
  B

  A
------ [+I1]
A + B

  B
------ [+I2]
A + B

A + B  A => C  B => C
---------------------- [+E]
          C


 F
---- [Efq]
 A 

A  A => B
--------- [=>E]
  B

-- A
------- [Raa]
A 


The introduction implication Rule =>I is not above. It corresponds to a Proof Line beginning with the word therefore.

This rule is defined on the syntax page
The conjunction is written &, the disjonction is written +
I = introduction, E = elimination,
=>E = modus ponens,
Efq = ex falso quodlibet, Raa = reductio ad absurdum
In addition to these rules, we define the negation and the equivalence by
-A = A => F
A <=> B = (A => B) & (B => A)
In a proof, one can replace every Formula by an other Formula equal when we replace the Negations and the Equivalences by their Definitions.
