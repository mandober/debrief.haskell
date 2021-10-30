# Domain and codomain of functions

Functions from-to
- from values to values (monomorphic)
- from types  to values (parametric polymorphism)
- from types  to types  (HKT)
- from values to types  (dependent types)

* Monomorphic or monomorphosized (i.e. regular) functions take a value and return a value; they are *functions from values to values*.

* Parametric polymorphism allows polymorphic functions that, in addition to value argument, take a type arg as their first, implicit (often hidden), argument. These are *functions from types to values*.

(∀x. λx. x)(Λℕ@n)

Whereas STLC has *variables ranging over terms*, and binders for them, System F additionally has *variables ranging over types*, and binders for them.


A dimension of the λ-cube corresponds to a new kind of dependency between terms and types; "dependency" refers to the capacity of a term or type to bind a term or type. The different ways to combine these 3 dimensions yield the 8 vertices of the cube, each corresponding to a different kind of typed system.The respective dimensions of the λ-cube correspond to:
- y-axis `↑` terms that can bind types, correspond to polymorphism
- x-axis `→` types that can bind terms, correspond to dependent types
- z-axis `↗` types that can bind types, correspond to (binding) type operators


* System F has explicit type abstraction and type application. 
For example, the familiar `map` function would be written:

```hs
map :: ∀a. ∀b. (a -> b) -> [a] -> [b]
map  = Λa. Λb. λ(f :: a -> b). λ(xs :: [a]).
    case xs of
        []     -> []
        (y:ys) -> f y : map @a @b f ys
```
