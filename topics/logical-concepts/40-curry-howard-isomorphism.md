## The Curry-Howard Isomorphism

**The Curry-Howard isomorphism** (CHI) is a correspondence between systems of formal logic, as encountered in proof theory, and computational calculi, as found in type theory.


The Curry-Howard Isomorphism  is the correspondence between the intuitionistic logic (IL) and simply typed lambda calculus (λ→).

It states that the type signatures of functions in λ→ correspond to logical propositions, and function bodies are equivalent to proofs of those propositions.

Stemming from CHI, the *proofs-as-programs* paradigm encompasses the approaches to developing programs, particularly in functional programming languages, based on the proofs in constructive, particularly intuitionistic, logic. Believed to hold great potential in realizing semi-automated software development tools.

This analogy, first discovered by Haskell Brooks Curry in the 1930's, has turned out to hold for other logical systems as well.

Minimal propositional logic corresponds to simply typed λ-calculus, first-order logic to λ-calculus with dependent types, second-order logic to polymorphic λ-calculus.

The isomorphism has many aspects, even at the syntactic level: formulas correspond to types, proofs to terms, provability to inhabitation, proof normalization to term reduction, etc.

It is an old idea of Brouwer, Kolmogorov and Heyting, later formalized by Kleene's realizability interpretation, that a constructive proof of an implication is a procedure that transforms proofs of the antecedent into proofs of the consequent. CHI formalizes this idea such that, e.g. proofs in the prepositional intuitionistic logic are represented by terms in a simply typed λ-calculus. Provable theorems are then merely non-empty types.

One of the central observations of the CHI is that an implication `A => B` in, preferably intuitionistic, logic corresponds to a function with signature `a -> b` in a strongly typed, preferably functional, programming language. In a proof derivation, the inference rule of *implication elimination* (also known as *modus ponens*) allows you to infer `B`, given both propositions `A => B` and `A`. Translated to a FPL, this is like applying a function `a -> b` to an argument of type `a` to produce a value of type `b`. So, implication elimination corresponds to function (lambda) application.

In the other direction, *implication introduction* is an inference rule that says that, if by assuming `A` you derive `B`, then you can conclude that `A => B`.

|- A -> B

1 | proposition 
2 | . assuming A
3 | 


n| . assuming A
…| . .  …
m| . .  B
k| therefore A => B (->I n-m)


by assuming `a` and then inferring `b`, corresponds to the introduction of a lambda abstraction that maps any element of a domain `a` into an element of a codomain `b`.
