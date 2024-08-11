# Lambda calculus :: TOPICS

lambda calculus
untyped lambda calculus
syntactic categories
  lambda term, Term
  lambda expression, exp
  lambda subexpression
  lambda abstraction, Abs
  lambda application, App
  varaible, Var
  lambda binder, λ
  implementation conventions
    binder contraction
    fixity: associativity and precedence
  scope of a binder
  variable
    bound
    free
  variable occurrencies
    binding vs applied
    bound vs free
  redex, reducable exp
  reduct, reduced exp
  combinator
  closed expression
  open expression
axioms, rules
  β-reduction
  α-conversion
  η-conversion
  η-reduction
  η-expansion
equivalence
  syntactic equality
  β-equivalence
  α-equivalence
  η-equivalence
  equivalence modulo renaming of bound variables
substitution
forms
  normal form, NF
  head normal form, HNF
  weak-head normal form, WHNF
Properties
  confluence
  normalization
    weak normalization (only normal order reduces the term)
    strong normalization (both appl and normal order reduce to the same term)
Divergence
  divergent exp
  self-application
  little omega, ω = λf.ff
  big omega, Θ = ωω
Fixpoint
  fixpoint of a function `f`: such `x` so that `f x = x`
  fixpoint calculator, `Fix f = x` for which `f x = x`
  If `x` is a fixpoint of `f`, then `f x` = `f (f x)` = `f (… (f x) …)` = `x`
    f x
    = f (f x)
    = f (f (f x))
    = f (f (f (f x)))
    = f (f (f (f (f x))))
    = f (… (f x) …)
    = x
  Fixpoint combinator
  Fixpoint of lamda exp, 𝕱 E = E (𝕱 E)




𝐅    𝐅 E = E (𝐅 E)
𝐹    𝐹 E = E (𝐹 E)
𝑭    𝑭 E = E (𝑭 E)
ℱ    ℱ E = E (ℱ E)
𝓕    𝓕 E = E (𝓕 E)
𝕱    𝕱 E = E (𝕱 E)
𝔽    𝔽 E = E (𝔽 E)
𝖥    𝖥 E = E (𝖥 E)
𝗙    𝗙 E = E (𝗙 E)
𝘍    𝘍 E = E (𝘍 E)
𝙁    𝙁 E = E (𝙁 E)
𝙵    𝙵 E = E (𝙵 E)
🄵    🄵 E = E (🄵 E)
🅕    🅕 E = E (🅕 E)
🅵    🅵 E = E (🅵 E)
