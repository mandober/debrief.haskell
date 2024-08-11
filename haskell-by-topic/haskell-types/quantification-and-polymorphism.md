# Quantification and Polymorphism in Haskell

- quantification
- universal quantification
- existential quantification
- relation between universal and existential quantification
  (situations when they are inter-expressible)
- value polymorphism, `0 :: Num a => a`, polymorphic literals
- type polymorphism
- parametric polymorphism, `forall a. a`
- constrained polymorphism, `forall a. Num a => a`
- instantiation
  - instantiating type var: at the call site *vs* in the definition
  - producer *vs* consumer, function's author *vs* function's caller
  - producing a polymorpic value *vs* handling a polymorpic value
- What is quantifiable in Haskell: types, kinds, constraints



...All of these functions have polymorphic types, which indicate that they work in a *uniform manner*, independently of the type [of elements in the lists].
