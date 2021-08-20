# Skolemization

*Skolemization* is the process of removing all existential quantifiers from a logic formula. The result is a formula in *Skolem normal form* that is equivalent in computational complexity to the original.

Skolemization is a way of removing existential quantifiers from a formula.
- Variables bound by existential quantifiers which are not inside the scope of universal quantifiers can simply be replaced by constants: `∃x[x<3]` can be changed to `c < 3`, with `c` as a suitable constant.
- When the existential quantifier is inside a universal quantifier, the bound variable must be replaced by a Skolem function of the variables bound by universal quantifiers. Thus `∀x[x = 0 ⋁ ∃y[x = y + 1]]` becomes 
`∀x[x = 0 ⋁ x = f(x) + 1]`.




Conversion of sentences FOL to CNF requires skolemization.

*Skolemization*: remove existential quantifiers by introducing new function symbols. *How*: for each existentially quantified variable introduce a n-place function where `n` is the number of previously appearing universal quantifiers. *Special case*: introducing constants (trivial functions: no previous universal quantifier).

The simplest form of Skolemization is for existentially quantified variables that are not inside the scope of a universal quantifier. These may be replaced simply by creating fresh constants. For example, `∃xP(x)` may be changed to `P(c)`, where `c` is a fresh constant (does not occur anywhere else in the formula).

More generally, Skolemization is performed by replacing every existentially quantified variable `y` with a term `f(x₁, …, xₙ)` whose function symbol `f` is fresh. The variables of this term are as follows: if the formula is in prenex normal form, then `xᵢ` are the variables that are universally quantified and whose quantifiers precede that of `y`. In general, they are the variables that are quantified universally (we assume we get rid of existential quantifiers in order, so all existential quantifiers before `∃y` have been removed) and such that `∃y` occurs in the scope of their quantifiers. The function `f` introduced in this process is called a *Skolem function* (or *Skolem constant* if it is of zero arity) and the term is called a *Skolem term*.

As an example, the formula `∀x∃y∀z[P(x,y,z)]` is not in Skolem normal form because it contains the existential quantifier `∃y`. Skolemization replaces `y` with `f(x)`, where `f` is a new function symbol, and removes the quantification over `y`. The resulting formula is `∀x∀z.P(x,f(x),z)`. The Skolem term `f(x)` contains `x`, but not `z`, because the quantifier to be removed `∃y` is in the scope of `∀x`, but not in that of `∀z`; since this formula is in prenex normal form, this is equivalent to saying that, in the list of quantifiers, `x` precedes `y` while `z` does not. The formula obtained by this transformation is satisfiable iff the original formula is.


## Skolemization: Example 1

"Every philosopher (P) writes (W) at least one book (B)". Note that `W(rites)` is a relation.

```
1 ∀p [ P(p) -> ∃b [B(b) ⋀ W(p,b)]]
2 ∀p [¬P(p) ⋁  ∃b [B(b) ⋀ W(p,b)]]           convert implication to disjunction
3 ∀p [¬P(p) ⋁ [B(sk(p)) ⋀ W(p, (sk(p))]]     skolemize: substitute b by g(p)
```

(1) we see that `∃b` is dependent on `∀p`, i.e. it is in the scope of `∀p`; this means skolemization will be realized via a function of `p` (and not via a constant).

(3) skolemize: substitute `b ⟼ sk(p)`


## Skolemization: Example 2

"All students of a philosopher read one of their teacher's books".

`∀p∀s[Philo(p) ⋀ StudentOf (s,p) -> ∃b[Book(b) ⋀ Write(p,b) ⋀ Read(s,b)]]`

```
1 ∀p∀s[ P(p) ⋀  S(s,p) -> ∃b[B(b) ⋀ W(p,b) ⋀ R(s,b)]]
2 ∀p∀s[¬P(p) ⋁ ¬S(s,p) ⋁  ∃b[B(b) ⋀ W(p,b) ⋀ R(s,b)]]
3 ∀p∀s[¬P(p) ⋁ ¬S(s,p) ⋁ [B(sk(p,s)) ⋀ W(p, sk(p,s)) ⋀ R(s, sk(p,s))]]
```

Skolemize (3): substitute `b ⟼ sk(p,s)`


## Skolemization: Example 3

"There exists a philosopher with students".

`∃p∃s[Philo(p) ∧ StudentOf (s,p)]`

```
1 ∃p∃s[P(p) ⋀ S(s,p)]
2      P(a) ⋀ S(b,a)
```

(2) Skolemize: substitute `p ⟼ a` and `s ⟼ b`


## First-order logic

*First-order logic* uses quantified variables over non-logical objects, and allows the use of sentences that contain variables. So, rather than propositions such as "Socrates is a man", one can have expressions in the form 
`∃x. Mx ⋀ Sx` (i.e. there exists `x` such that `x` is a man and `x` is Socrates) where `∃` is the existential quantifier, while `x` is a variable. This distinguishes it from propositional logic, which does not use quantifiers or relations; in this sense, propositional logic is the foundation of first-order logic.

A theory about some particular mathematical topic is usually a first-order logic together with a specified domain of discourse (over which the quantified variables range), finitely many functions from that domain to itself, finitely many predicates defined on that domain, and a set of axioms believed to hold about them. Sometimes, "theory" is understood in a more formal sense, which is just a set of sentences in first-order logic.

The adjective "first-order" distinguishes FOL logic from HOL (higher-order logic), in which there are predicates having predicates or functions as arguments, or in which predicate quantifiers or function quantifiers or both are permitted. In first-order theories, predicates are often associated with sets. In interpreted higher-order theories, predicates may be interpreted as sets of sets.

FOL is the standard for the formalization of mathematics into axioms, and is studied in *the foundations of mathematics*. Peano arithmetic and ZF set theory are axiomatizations of number theory and set theory, respectively, into FOL. No first-order theory, however, has the strength to uniquely describe a structure with an infinite domain, such as the natural numbers or the real line. Axiom systems that do fully describe these two structures (that is, categorical axiom systems) can be obtained in stronger logics such as *second-order logic*.

While propositional logic deals with simple declarative propositions, first-order logic additionally covers predicates and quantification. A predicate takes an entity or entities in the domain of discourse as input while outputs are either True or False.





---

## Ref

https://en.wikipedia.org/wiki/Bounded_quantifier
https://en.wikipedia.org/wiki/First-order_logic
https://en.wikipedia.org/wiki/Free_variables_and_bound_variables
https://en.wikipedia.org/wiki/Generalized_quantifier
https://en.wikipedia.org/wiki/Higher-order_logic
https://en.wikipedia.org/wiki/Prenex_normal_form
https://en.wikipedia.org/wiki/Quantifier_(logic)
https://en.wikipedia.org/wiki/Quantifier_rank
https://en.wikipedia.org/wiki/Quantifier_elimination
https://en.wikipedia.org/wiki/Second-order_logic
https://en.wikipedia.org/wiki/Skolem_normal_form
https://en.wikipedia.org/wiki/Universal_quantification
https://en.wikipedia.org/wiki/Well-formed_formula




Converting predicate logic formulas into Skolem Normal Form
https://math.stackexchange.com/questions/2183166/converting-predicate-logic-formulas-into-skolem-normal-form

FOL
https://crypto.stanford.edu/~blynn/compiler/fol.html

why Skolemization?
https://mathoverflow.net/questions/114083/why-skolemization

getting rid of existential quantifiers
https://mathoverflow.net/questions/90324/getting-rid-of-existential-quantifiers

Skolemization
https://planetmath.org/skolemization

Skolemization
https://www.youtube.com/watch?v=dGyz1dgTx0I
https://www.youtube.com/watch?v=NFHkcB1THFc

Skolemization: demo
https://demonstrations.wolfram.com/Skolemization/

---

Skolemization, Most General Unifiers, First-Order Resolution
CSC 384, University of Toronto, March 07, 2011, prof. Torsten Hahmann
https://www.cs.toronto.edu/~sheila/384/w11/Lectures/csc384w11-KR-tutorial.pdf

About proof-search in intuitionistic natural deduction calculus using
partial Skolemization
https://iopscience.iop.org/article/10.1088/1742-6596/1680/1/012038/pdf

3.5 Normal Forms and Skolemization (Traditional)
http://rg1-teaching.mpi-inf.mpg.de/autrea-ss10/script/lecture10.pdf

First Order Logic: Prenex normal form. Skolemization. Clausal form
Valentin Goranko, DTU Informatics, September 2010
http://www2.imm.dtu.dk/courses/02286/Slides/FirstOrderLogicPrenexFormsSkolemizationClausalFormTrans.pdf
