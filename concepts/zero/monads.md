# Monads

Monads are ideal for dealing with side-effects that include:
- partial functions
- shared read-only environment
- manipulating state
- indeterminism

impure computations   | pure          | embellishment  | type ctor   
----------------------|---------------|----------------|-------------
a → **IMPURITY**  → b | monad pattern | `a → m a`      | `m`         
a → partials      → b | partial fns   | `a → Maybe a`  | `Maybe a`   
a → env           → b | environment   | `a → (r → a)`  | `Reader r a`
a → state         → b | state         | `a → (s → a)`  | `State  s a` 
a → indeterminism → b | indeterminism | `a → [a]`      | `[] a`      


This table illustrates the following: we often deal with impure procedures whose types fit into the signature scheme of `a -> b`. However, we cannot represent impure computations in Haskell and the question of how could it be done has been a longstanding one until the work of Moggie who showed that the impure computations can be described and represented using monads from category theory.

All of these impure procedures we've mentioned (partial fn, env, state, indeterminism) can, in fact, be described with monads: their signature that fits `a -> b` is converted to the pure `a -> m b`, then specizlized for each case. They all take some type `a` and they produce, not type `b`, but an **embellished type** `b`. They are embellished computations.

```
a -💩-> b  General signature of impure computations
a -> m b   General signature of pure (embellished) computations

Partial functions:
   💩 stands for partial functions - we use Maybe monad: Maybe a
   a -> Maybe a

Environment:
   💩 is read-only env - we use the Reader monad: Reader r a
   Reader e a is just wrapping the type: a → r → a
   so, in a way, 💩 :: r

State:
   💩 is state - we use Maybe monad: Maybe a
   a -> Maybe a

Indeterminism:
   💩 is indeterminism - we use list monad: [a]
   a -> [a]
```
