# Parametricity

* Parametricity is just restated *Reynold's abstraction theorem*: 
terms evaluated in related environments produce related values.

* *The Parametricity Theorem*. If `a: t` is a closed term, 
then `(a, a) ∈ Relₜ`, i.e. every term is related to itself.

* *Theorems for free* refers to deriving theorems about functions from their types only. An easy theorem-for-free asserts that the type `∀(X)X → Bool` i.e. `∀a. a -> a -> Bool` necessarily contains only constant functions.

* The results that allows theorems to be derived from types is referred to as *the parametricity result* because it depends on parametric polymorphism, i.e. types of the form `∀a.T`
