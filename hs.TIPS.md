# Haskell :: Tips

- Types are abstraction of data (values)
- type signatures first, implementation (definition) later
- Just follow the types (follow the money)
- Make invalid states unrepresentable
- apply in its various forms: $, <$>, <*>, >>=
- composition in its various forms: (.), (:.), >=>, apply-combinators
- recreate stdlib's functions with help from predicate logic: lambda introduction equals new assumption, application equals MP; follow the types.
- on encountering a context `m` instantiate `m` with list, Maybe, … in yr head
- parameteric polymorphism: the broader the type, the less you can do with it (think intersection of types).
- ad hoc polymorphism: same name - very different implementation! However, using these same names, we get to code generically: fmap is different at any type, has a different implementation.
- in dire straits use fmap




https://github.com/epoberezkin/poberezkin.com/blob/master/posts/2021-04-21-what-i-wish-somebody-told-me-when-i-was-learning-Haskell.md
