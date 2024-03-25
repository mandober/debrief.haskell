# Mutability

https://www.snoyman.com/reveal/what-makes-haskell-unique/
https://www.snoyman.com/reveal/what-makes-haskell-unique-2020/#/15

## Immutability and purity

- Most languages default to mutable values
- Haskell differs in two ways:
  - Immutable by default, explicit kind of mutability
  - Mutating is an effect, tracked by the type system

Summary: immutability and purity
- Advantages 
  - Easier to predict code behavior
  - Avoid many cases of data races
  - Functions are more reliable, returning the same output for the same input
- Disadvantages 
  - Lots of ceremony if you actually want mutation
  - Some runtime performance hit for mutable algorithms

## Laziness
https://www.snoyman.com/reveal/what-makes-haskell-unique/#/4/7
https://www.snoyman.com/reveal/what-makes-haskell-unique-2020/#/
https://www.snoyman.com/reveal/what-makes-haskell-unique-2020/#/32

Advantages
- More composable code
- Get efficient results with high level code
- Short-circuiting no longer a special case

Disadvantages
- Need to worry about space leaks
- Laziness means an exception can be hiding in any thunk
- Bad functions still linger
  - Aka partial functions: head []
  - Also, some inefficient functions still available, foldl vs foldl'

Laziness is very similar to features in other languages
- Python generators
- Rust iterators

In Haskell, it's far more prevalent since it affects how all code works. However, you can get a lot of the benefits of Haskell with these techniques
