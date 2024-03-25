# Algebra of types


## Refs

Abusing the algebra of algebraic data types - why does this work?
https://stackoverflow.com/questions/9190352/abusing-the-algebra-of-algebraic-data-types-why-does-this-work

The algebra (and calculus!) of algebraic data types - Joel Burget
https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types


https://en.wikipedia.org/wiki/Generalized_algebraic_data_type

https://web.archive.org/web/20140122211018/http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/

https://web.archive.org/web/20140222144454/http://chris-taylor.github.io/blog/2013/02/11/the-algebra-of-algebraic-data-types-part-ii


## Introduction

Algebraic data types (ADTs) are fundamental to Haskell and other FPLs. They are the building blocks for constructing more complex types, such as lists, sets, maps, trees, graphs.

ADTs and algebra have some operations. Type theory uses the algebraic notation to define ADTs, rather then some syntax similar to Haskell's.

### Nominal form

Haskell's product types have nominal form that takes the form of a tuple, although, as a convenience, the types are instead writen as juxtaposition, i.e.without commas and, possibly, using less parenthesis (unless you insist).

These pairs are equivalent:
- `... Cons a (List a)` ≅ `... Cons (a, List a)`
- `... Cons (List a) a` ≅ `... Cons (List a, a)`
- `... Branch (Tree a) a (Tree a)` ≅ `... Branch (Tree a, a, Tree a)`


### Canonical forms

- The canonical sum type in Haskell is `Either a b`
- The canonical product type is a pair, `(a,b)`
- All types can be expressed as sums of products


## Correspondance

Algebra     Haskell                         (Logic)
0           Void                            p -> False
1           unit (a bare ctor)              p
+           sum type                        or, disjunction
×           product type                    and, conjunction
^           exponential a -> b <=> bᵃ       implication
​​

data Void                 0
data Unit = Unit          1
data Bool = T | F         2
(a, b)                    a × b
Either a b                a + b




## Haskell examples

```
data Maybe a = Nothing | Just a                     M = 1 + a
data List a = Nil | a : List a                      L = 1 + L × a
data List a = Nil | (a, List a)                     L = 1 + L × a
data List a = Nil | (List a, a)                     L = L × a + 1
data Tree a = Nil | Branch (a, Tree a, Tree a)      T = 1 + a × T × T
```


## Hask Category

https://stackoverflow.com/questions/9190352/abusing-the-algebra-of-algebraic-data-types-why-does-this-work

https://wiki.haskell.org/Hask

* Hask is the category of Haskell types and functions.

* The objects of Hask are Haskell types, and the morphisms from objects A to B are Haskell functions of type A -> B. The identity morphism for object A is `id :: A -> A`, and the composition of morphisms f and g is `f . g = \x -> f (g x)`.

* If we ignore bottom, Haskell types form a category 𝗛𝗮𝘀𝗸.

* Because of these difficulties (bottom), Haskell developers tend to think in some subset of Haskell where types do not have bottom values. This means that it only includes functions that terminate, and typically only finite values. The corresponding category has the expected initial and terminal objects, sums and products, and instances of Functor and Monad really are endofunctors and monads.




Product           | Coproduct
------------------|--------------------
product           | coproduct
A×B               | A+B
fst : A×B → A     | inl : A → A+B
snd : A×B → B     | inr : B → A+B
f : C → A         | f : A → C
g : C → B         | g : B → C
pairing           | copairing
f ∧ g : C → A×B   | f ∨ g : A+B → C
fst . (f ∧ g) = f | inl . (f ∨ g) = f
snd . (f ∧ g) = g | inr . (f ∨ g) = g




**Product**

Given types (objects) A and B in Hask, 
their product A×B is the unique type 
(up to an isomorphism) 
that allows two projections:            | injections:
  fst : A×B → A                         | inl : A → A+B
  snd : A×B → B                         | inr : B → A+B
where 
given any (other) type C 
and two functions 
  f : C → A                             | f : A → C
  g : C → B                             | g : B → C
you can define the pairing              | copairing
  f &&& g : C → A×B                     | f ||| g : A+B → C
such that 
  fst ∘ (f &&& g) = f                   | inl ∘ (f ||| g) = f
  snd ∘ (f &&& g) = g                   | inr ∘ (f ||| g) = g

Parametricity guarantees the universal properties automatically. 
The (&&&) operator is in Control.Arrow.


**Coproduct**

The dual of the above is the 
coproduct A+B with injections 
inl : A → A+B
inr : B → A+B
where
given any type C
and functions
f : A → C
g : B → C
you can define the copairing 
f ||| g : A+B → C
such that 


the obvious equivalences hold.

Again, parametricity guarantees most of the tricky parts automatically.
In this case, the standard injections are simply `Left` and `Right` 
and the copairing is the function `either`.
