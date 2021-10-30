# Kinding

- the kind `*`, also called *ground*, is 0th order
- a kind of the form `* -> … -> *` (with at least one arrow) is 1st order
- *higher-order kind* is a kind with a nested arrow before another arrow, 
`(* -> *) -> *`

* A kind's order is the depth of the nested arrows on the LHS, e.g. 
 `(* -> *) -> *`        is 2nd order, 
`((* -> *) -> *) -> *`  is 3rd order, etc.

order | kind                      | notes
------|---------------------------|------------------------------------------
0ᵗʰ   | *                         | the ground kind, aka Type
1ˢᵗ   | * -> *                    | 2-ary (binary) kind arrow
1ˢᵗ   | * -> * -> *               | 3-ary (ternary) kind arrow
1ˢᵗ   | * -> … -> *               | n-ary kind arrow (min 1 arrow)
HKT   |  (* -> *) -> *            | nested arrow on the LHS of an arrow
2ⁿᵈ   |  (* -> *) -> *            | 2nd order
3ʳᵈ   | ((* -> *) -> *) -> *      | 3rd order 



* The same concept applies to types: a 2nd order function is one with the signature `(a -> b) -> c`.

* The same concept also applies to forall-types, except in this case the order is replaced by **rank**. Higher-rank types are functions that take polymorphic functions as their args. 
A Rank-1 type has a forall-type before an 
arrow , `forall a. a -> a` but this one is also Rank-1: 
`Int -> (forall a. a -> a)`
A Rank-2 type has a forall-type before 2 arrows, 
`(forall a. a -> a) -> Int`.

Examples:
- `Int -> Int`                Rank-0 (no type params)
- `forall a. a -> a`          Rank-1 (depth 1, `∀a` is before 1 arrow)
- `Int -> (forall a. a -> a)` Rank-1 (depth 1, `∀a` is before 1 arrow)
- `(forall a. a -> a) -> Int` Rank-2 (depth 2, since `∀a` is before 2 arrows)
