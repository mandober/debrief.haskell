# Dependent Types in Haskell

## Dependent function space

Using polymorphism, we can avoid writing the same function on, say, integers and Booleans. We can define polymorphic functions, such as the identity:

```hs
id :: ∀α.α → α
id = λx → x
```

When such expressions are translated to GHC's core language, *the polymorphism does not disappear* - instead, the identity function in the core takes two arguments: a type `α` and a value of type `α`. Calls to the identity function in the core, must explicitly instantiate the identity function with a type:

```hs
id Bool True :: Bool
id Int 3 :: Int
```

Haskell's polymorphism allows types to abstract over types. Consider the following data types:

```hs
data Vector1 α = Vector1 α
data Vector2 α = Vector2 α α
data Vector3 α = Vector3 α α α
```

Clearly, there is a pattern here; we'd like to give it a type with the kind

`∀α :: ∗. ∀n :: Nat. Vec α n`

but this doesn't type in Haskell. The problem is that the *type* `Vec` abstracts over the *value* `n`.

> The dependent function space `∀` generalizes the usual function space `→` by allowing the range to depend on the domain.

The parametric polymorphism can be seen as a special case of a dependent function, motivating our use of the symbol `∀`. Type theorists call dependent function types *Π-types* and would write the type signature for Vec as

`Πα : ∗. Πn : Nat. Vec α n`

In contrast to polymorphism, the *dependent function space* can abstract over more than just types. The `Vec` type is a valid dependent type.

It is important to note that *the dependent function space is a generalization of the usual function space*. For instance, we can type the identity function on vectors as

`∀α :: ∗. ∀n :: Nat. ∀v :: Vec α n. Vec α n`

Note that the type `v` doesn't occur in the range: this is simply the non-dependent function space already familiar to Haskell programmers. Rather than introduce unnecessary variables, such as `v`, we use the ordinary function arrow for the non-dependent case. The identity on vectors then has the following, equivalent, type:

`∀α :: ∗. ∀n :: Nat. Vec α n → Vec α n`

In Haskell, one can sometimes fake the dependent function space, e.g. by defining Nat on the type level (using, e.g. data promotion). Since the type level numbers are different from the value level natural numbers, we end up duplicating a lot of concepts on both levels.

Furthermore, even though one can lift certain values to the type level in this fashion, an additional effort (in the form of advanced type class programming) is required to perform computations on such types. But using dependent types, we can parameterize our types by values, so the normal evaluation rules apply.

```hs
Πα :  ∗. Πn :  Nat. Vec α n
∀α :: ∗. ∀n :: Nat. Vec α n

-- identity function on vectors
∀α :: ∗. ∀n :: Nat. ∀v :: Vec α n.   Vec α n
∀α :: ∗. ∀n :: Nat.       Vec α n -> Vec α n
-- Rather than introduce unnecessary vars, such as `v`, we use the regular function arrow for the non-dependent case. The identity on vectors then has this equivalent type (right above).
```
