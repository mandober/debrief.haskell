# About functions in Haskell

Total function are defined for all inputs, for input values of all types.

The most magnificent total function is `id`. It is the only function defined for any input, past, present or future. The `id` is *super-total function*, it never fails! The `id :: a -> a` is **truly polymorphic**. Another super-total function is `const :: a -> b -> a`. Other super-polymorphic functions could be defined using this strategy of taking whatever and just returning it or ignoring it, or some variation, e.g. `second :: a -> b -> c -> b`, or `pack13 :: a -> b -> c -> (a, c)`.


Other total polymorphic functions will, of course, fail if the input is of inappropriate type
