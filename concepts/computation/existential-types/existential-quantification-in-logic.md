# Existential quantification in logic

**Quantification** is the act of specifying the number of the individuals in the domain of discourse which are affected by a variable (that ranges over the domain) in a predicate.

A **quantifier** is a binder that declares and quantifies a variable that ranges over the domain of discourse.

There are two types of quantification in logic:
- univeral quantification, `∀`
- existential quantification, `∃`

For example, if the domain of discourse are people, `M`, then a variable `x` ranging over `M`, may be quantified to affect all people, `∀x(x ∈ M)`, or some people (at least one person), `∃x(x ∈ M)`. Actually, logic prefers to express set membership, `x ∈ S`, as a predicate `S(x)` - if the predicate `S x` is satisfied (holds) then `x` indeed belongs to the set `S`.

Composing the predicates we get the two common forms of quantification:
- universal:   `∀x(S x ⇒ M x)`
- existential: `∃x(S x ∧ M x)`


In a predicate "X is a man", where X is a variable that inhabits a particular domain of discourse (i.e. the type of the variable), and X has to be quantified in some way. This predicate can be interpreted as a unary function with type `Animal -> Bool`. A function in this sense, is a *predicate* in logic programming languages.

To quantify X, we can either universally or existentially quantify the variable. To *universally quantify* it, means to say `is a man` is a property that holds for all members of the type `Animal`. To *existentially quantify* it means to say it only holds for some members (at least one) of the type `Animal`.

We can denote these two forms by the formulas

- `∀x(Animal x ⇒ Man x)`   
  for all `x` that are members of the `Animal` set, `x` is a man; 
  i.e. if `x` is an `Animal`, then `x` is a `Man` (dependent properties)

- `∃x(Animal x ∧ Man x)`   
  for some `x` that are in `Animal` set, `x` are in `Man` set; 
  i.e. `x` is an `Animal` and `x` is a `Man` (orthogonal properties)


Every predicate expressed in a natural language is always quantified, although quantification is often implicit relying on the conversational context.

For example, "the glasses in my recent order were chipped" vs "every glass in my recent order was chipped" and "some glasses in my recent order were chipped".
