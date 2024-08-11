# Haskell :: Index :: Polymorphism in Haskell

+ term polymorphism
+ type polymorphism
+ kind polymorphism
+ constraint polymorhism
- parameteric polymorphism, universal polymorphism
- let-polymorphism
- ad hoc polymorphism
- row polymorphism ✘
- levity polymorphism (lifted vs unlifted types)
- runtime-rep polymorphism
- multiplicity polymorphism (linear vs regular fucntions)
- coherence polymorhism (?)
- Intensional Static Polymorphism - "Kinds Are Calling Conventions" paper
- Arity Polymorphism
- "representation of values" polymorphism
- "the rarity of machine functions" polymorphism
- "the evaluation order of arguments" polymorphism, polymorphic evaluation



## Intensional Static Polymorphism - "Kinds Are Calling Conventions" paper

Polymorphism over not only the types of values, but also the representation of values, the rarity of machine functions, and the evaluation order of arguments - all 3 of which are useful in practice. The key insight is to encode information about a value's calling convention in the kind of its type, rather than in the type itself.



```hs
-- Generalize (a :: ★) to (a :: TYPE r)
-- (r :: Rep) is the representation of a
-- ★ = TYPE Ptr
error :: forall (r :: Rep) (a :: TYPE r). String -> a
```
