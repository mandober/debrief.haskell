# Polytypic programming

From Polymorphism To Polytypism - Nicolas Wu, 2010
https://zenzike.com/posts/2010-12-10-from-polymorphic-to-polytypic.html

## Type-indexed data types

(from the paper: `Type-indexed data types`, 2004 by R.Hinze, J.Jeuring, A.Loh)

A *polytypic function* is a function that can be instantiated at many types to obtain type-specific functionality. In Haskell, examples of polytypic functions are derivable class methods including `show`, `read`, `==`.

Additional examples are functions intended for digital searching, pattern matching, unification, rewriting, structure editing. For each of these issues, we not only have to define polytypic functionality, but also a type-indexed data type.

A *type-indexed datatype* is a datatype constructed generically from an argument datatype. For example, in the case of digital searching, we have to define a search tree type by induction on the structure of the type of search keys.

This paper shows how to define type-indexed data types, discusses several examples of type-indexed data types, and shows how to specialize type-indexed data types. The approach has been implemented in Generic Haskell, a generic programming extension of the functional language Haskell.


The central idea of **polytypic programming** (also called *type-indexed* or *generic programming*) is to provide the programmer with the ability to define a function by induction on the structure of types.

### Algebraic types

Since Haskell's type system is rather advanced (mutually recursive types, parameterized types, nested types, type constructors of higher-order kinds) this sounds complex; fortunately, it can be shown that a *polytypic function is uniquely defined* by giving cases for a very limited set of types and type constructors.

For instance, to define a generic function on types of kind `*`, we need cases for the unit type (1), the sum type ctor (+), and the product type ctor (×). These 3 types are required for modelling Haskell's data construct that introduces a *sum of products*. We treat `1`, `+`, `×` as if they were given by these declarations:

```hs
data 1 = ()
data a + b = Inl a | Inr b
data a × b = (a, b)
```

If we want our generic functions to work on primitive types (that are not defined by means of a Haskell `data` statement) we need to include additional cases for such types; for simplicity, we will assume `Char` to be the only base type.

Now, a polytypic function is given by a definition that is inductive on `1`, `Char`, `+`, and `×`.

As an example, here is the polytypic equality function; for emphasis, the type index is enclosed in braces.

```hs
-- sig
equal {t :: *>}        :: t -> t -> Bool

-- unit is always equal to itself
equal {1}       ()       ()       = True

-- two Chars are equal if their primOp for eq says so
equal {Char}    c1       c2       = equalChar c1 c2

-- two sum types, (L a₁ | R b₁) and (L a₂ | R b₂), are equal if:
equal {t₁ + t₂} (Inl a₁) (Inl a₂) = equal {t₁} a₁ a₂    -- a ~ a₁ ~ a₂
equal {t₁ + t₂} (Inl a₁) (Inr b₂) = False
equal {t₁ + t₂} (Inr b₁) (Inl a₂) = False
equal {t₁ + t₂} (Inr b₁) (Inr b₂) = equal {t₂} b₁ b₂    -- b ~ b₁ ~ b₂

-- two pairs are equal: (a, b) = (x, y) <=> (a = x) ⋀ (b = y)
equal {t₁ × t₂} (a₁, b₁) (a₂, b₂) = equal {t₁} a₁ a₂ && equal {t₂} b₁ b₂
```

This simple definition contains all ingredients needed to specialize `equal` to arbitrary data types.

Note that the definition does not mention type abstraction, type application, and fixed points. Instances of polytypic functions on types with these constructions can be generated automatically from just the cases given above.

For example, if we used `equal` at the data type `Bush`, the generated specialization would behave exactly as the following hand-written code:

```hs
equal Bush :: Bush -> Bush -> Bool
equal Bush (Leaf c1) (Leaf c2)       = equalChar c1 c2
equal Bush (Fork m1 r1) (Fork m2 r2) = equal Bush m1 m2 && equal Bush r1 r2
equal Bush                           = False
```

Sometimes we want to be able *to refer to the name of a ctor*. To this end, we add one more special type ctor for which a case can be defined in a generic function: `c of t`, where `c` is a value, and `t` is a type of kind `*`.

The value `c` represents the name of a ctor. If the `c of t` case is omitted in the definition of a polytypic function `poly`, as in function `equal`, we assume that `poly {c of t} = poly {t}`. For the purposes of this paper, we assume that `c :: String`.

As an example for the use of ctor names in a generic function, we give a very simple variant of the polytypic `show` function, that computes a textual representation of any value:

```hs
show {t :: *} :: t -> String
show {1}       ()      = ""
show {Char}    c       = showChar c
show {t₁ + t₂} (Inl a) = show {t₁} a
show {t₁ + t₂} (Inr b) = show {t₂} b
show {t₁ × t₂} (a, b)  = show {t₁} a ++ " " ++ show {t₂} b
show {c of t}  t       = "(" ++ c ++ " " ++ show {t} t ++ ")"
```
