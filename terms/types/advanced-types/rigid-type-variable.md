# Rigid type variable

Consider the following function:

```hs
gee :: forall a b. a -> a -> b
gee x y = (x, y)
```

This doesn't compile and the error message is:

Couldn't match expected type `b` with actual type `(a,a)`,    
`b` is a rigid type variable bound by the type signature for:   
`gee :: forall a b. a -> a -> b`

Why does it say that `a` is a rigid type variable despite the fact that `a` can stand for any type at all? Shouldn't it be called very flexible?

`a` is indeed very flexible, but only from the perspective of the function's caller. In the function's definition, `a` is not flexible at all. Within the function's body `a` must be treated as an unknown, but fixed type, and the function must do exactly the same thing for all possible values i.e. values of any type (since `a` stands for any type).


---

Rigid types are mentioned in a 2004's paper *"Wobbly Types: Type Inference For Generalised Algebraic Data Types"* by `Simon Peyton Jones, Geoffrey Washburn, Stephanie Weirich`.

* **Wobbly types** are types that the compiler has to infer.

* **Rigid types** are types that are completely specified by the author, in some direct manner, e.g. in a signature or by annotations.

The name "user-specified type" was also considered but "rigid type" stuck as a shorter one. In fact, the name *user-specified type* is more intuitive in indicating that a type variable will finally be unified with a concrete type when it is used i.e. when the caller supplies a type.

For a full discussion of the precise meaning of "rigid type variable" and why this phrase was chosen, please see the paper *"Simple unification-based type inference for GADTs"* by `Dimitrios Vytiniotis, Stephanie Weirich, and Simon Peyton Jones`.
