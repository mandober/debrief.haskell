# Monads

Monads were introduced by Godement in algebraic topology in 1958. They became referred to as standard construction and triples before Saunders MacLane popularized the name monad and also introduced the phrase "a monad is a monoid in the category of endofunctors".

The following definition of a monad is taken from a paper by Moggi that first used monads in the context of programming languages:

> Definition 1: 
A monad over a category 𝓒 is a triple (T, η, µ) where T : 𝓒 -> 𝓒 is a functor, η : Idᴄ -> T and µ : T² -> T are natural transformations such that:

µᴀ ◦ Tµᴀ = µᴀ ◦ µᴛᴀ     
µᴀ ◦ Tηᴀ = µᴀ ◦ ηᴛᴀ = idᴛᴀ

(T as a polymorphic type, like a list, natural transformations η and µ as functions and T² as a nested polymorphic type)

Originally, monads were used to embed an object into an object with a richer structure and also to express many different constructions in terms of the same structure.

E. Moggi used monads in semantics of programming languages to capture many different notions of computation that go beyond total functions, such as nondeterminism, side-effects and exceptions. His paper provides categorical definitions for several monads later used in programming, however, it does not introduce them as programming tools, but rather as proving tools: "This paper is about logics for reasoning about programs, in particular for proving equivalence of programs".

P. Wadler is the first to use monads as programming tools. Interestingly, similar structure was independently proposed by Spivey, although without a reference to monads. Moggi introduced monads for reasoning about effectful programs, but Wadler uses them to implement effectful programs in a purely FPL. Wadler has translated categorical definition to a FP, where a functor becomes a type constructor with a map function and natural transformations become functions.

> Definition 2:
A monad is an operator M on types, together with a triple of functions (map, unit, join), satisfying a specific set of axioms.

Triple of functions:
* `map      :: (x -> y) -> M x -> M y`
* η = `unit :: x -> M x`
* µ = `join :: M (M x) -> M x`

Monad axioms:
1. `join` ◦ join = `join` ◦ map join            `µᴀ ◦ Tµᴀ = µᴀ ◦ µᴛᴀ`
2. `join` ◦ unit = `join` ◦ map unit = id       `µᴀ ◦ Tηᴀ = µᴀ ◦ ηᴛᴀ = idᴛᴀ`
3. map id = id
4. map (g ◦ f) = map g ◦ map f
5. map f ◦ join = join ◦ map (map f)
6. map f ◦ unit = unit ◦ f

The natural transformation `µ` is join, `η` is unit. The first two monad laws are direct translation of the laws in Definition 1. The remaining laws are properties of natural transformations.

In programming, an alternative, but equivalent, definition of monads became popular shortly after the original appeared.

> Definition 3: 
A monad is a triple (M, unit,>>=) consisting of a type constructor M and
two operations of the following types:
* `(>>=) :: M x -> (x -> M y) -> M y`
* `unit  :: x -> M x`

These operations must satisfy the following laws:
1. `unit a >>= f = f a`
2. `m >>= unit = m`
3. `(m >>= f ) >>= g = m >>= (λx.f x >>= g)`

The (>>=) operator, known as `bind`, was initially written with the params flippd and referred to as (★), but now it is (=<<)
* `(>>=) :: M x -> (x -> M y) -> M y`
* `(=<<) :: (x -> M y) -> M x -> M y`


```
unit :   a   -> [a]
join : [[a]] -> [a]
map  : (f :: a -> b)   => [a] ~~map f~~> [b]
bind : (g :: a -> [b]) => [a] ~~map g~~> [[b]] ~~join~~> [b]
```

unit (unit x) ~~> [[x]]
join (unit (unit x)) ~~> [x]

The bind operation explained as a combination of map and join: it takes a function that produces a box, applies it to all values contained in another box and then unwraps the nested boxes.


---

## Notions of computation and monads
by Eugenio Moggi, 1991

A few examples of computation in the category of sets:
- *partiality* 
  TA = A⊥ (i.e. A + {⊥}) 
  where ⊥ is a diverging computation
- *nondeterminism* 
  TA = 𝓟 ꜰɪɴ(A)
- *side-effects* 
  TA = (A × S)ˢ 
  where S is a set of states, e.g. a set Uˡ of stores 
  or a set of input/output sequences U∗
- *exceptions* 
  TA = (A + E), where E is the set of exceptions
- *continuations* 
  TA = R^(R^A) 
  where R is the set of results
- *interactive input* 
  TA = (µγ.A + γᵘ ) 
  where U is the set of characters. More explicitly TA is the set of U-branching trees with finite branches and A-labelled leaves
- *interactive output* 
  TA = (µγ.A + (U × γ))
  More explicitly TA is (isomorphic to) U∗ × A


We go through the examples of computations and show that many tasks can be modelled as Kleisli triples.

* *partiality*
  TA = A⊥ (= A + {⊥})
  ηA is the inclusion of A into A⊥
  if `f: A → TB` then `f∗(⊥) = ⊥` and `f∗(a) = f(a)` (when a ∈ A)
