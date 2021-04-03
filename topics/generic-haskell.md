# Generic Haskell

(Generic Haskell: practice and theory - Ralf Hinze and Johan Jeuring, 2003)

Generic programming aims at making programming more effective by making it more general. Generic programs often embody non-traditional kinds of polymorphism.

*Generic Haskell* is an extension of Haskell that supports the construction of generic programs. Generic Haskell adds to Haskell the notion of **structural polymorphism**, the ability to define a function (or type) *by induction on the structure* of types. Such a function is generic in the sense that it works not only for a specific type but for a whole class of types. Typical examples include equality, parsing, pretty printing, serialising, ordering, hashing.


## Introduction

*Safe language*. Safety can be achieved by static type checking, by dynamic
type checking, or by their combination, which is true in Haskell's case; e.g. passing an integer to a list-processing function is captured statically at CT while taking the first element of the empty list results in a RT error.

*Static typing*. The problem with static type systems is that they are always too conservative: they must necessarily reject some portion of programs that behave correct at RT (which is better than the alternative: accepting some incorrect programs). In a sense, generic programming is about extending the boundaries of static type systems and allowing more programs to type check.

*Polymorphic type systems*. Polymorphism complements type security by flexibility. Polymorphic type systems like the Hindley-Milner system allow the definition of functions that behave uniformly over all types. A standard example is the function `length :: ∀a. [a] -> Int`, which is insensitive to the type of elements, which is signalled by the universal quantifier. Quantifiers come from logicand there is a close correspondence between polymorphic type systems and systems of higher-order logic. In light of this correspondence, the quantifier in length's signature is *second-order* as it ranges over sets (equating types with sets).

However, even polymorphic type systems are sometimes less flexible than desired. For instance, it is not possible to define a polymorphic equality function that works for all types.

```hs
-- polymorphic fn
length :: ∀a. [a] -> Int

-- no good, doesn't work
eq :: ∀a. a -> a -> Bool
```

The *parametricity theorem* implies a function of type `∀a. a -> a -> Bool` must necessarily be *constant*. As a consequence, the programmer is forced to write a separate equality function for each type. This sounds like a simple task but may, in fact, be arbitrarily involved.

## Data type declaration

Haskell offers data type declaration for defining new types; it simultaneously introduces a new type constructor B and n data constructors K1...Kn.

```hs
data B a1...am = K1 t11...t1m1 |...| Kn tn1...tnmn

Kj :: ∀a1...am. tj1 -> ... -> tjmj -> B a1...am
```

* The type parameters a1...am must be distinct and may appear on the righthand side of the declaration. If m > 0, then B is called a *parameterized type*.

* Data type declarations can be recursive with B appearing on the RHS. In general, data types are defined by a system of *mutually recursive data type declarations*.

* A Haskell data type is essentially a *sum of products*: the components of the sum are labelled by constructor names; the arguments of a constructor form a product.


### Finite types

* Data type declarations subsume enumerated types by only having nullary data ctors. Data type declarations also subsume record types.

* Haskell assigns a *kind* to each type ctor (the "type" of a type ctor). The `*` or `Type` kind represents nullary ctors (Char, Bool). The kind `k -> v` represents type ctors that map type ctors of kind `k` to those of kind `v`.

```hs
-- enumerated types
type Bool :: *
data Bool = False | True

-- record types
type Fork :: * -> *
data Fork a = Fork a a

type Maybe :: * -> *
data Maybe a = Nothing | Just a
```

### Recursive types

Data type declarations may be recursive or even mutually recursive.

Strings can also be represented by a recursive data type. The type String is a binary sum: the first summand, `Nil`, is a nullary product and the second summand, `Cons`, is a binary product. The type of parametric lists is obtained from `String` by abstracting over `Char`.

In the `Tree` data type we distinguish between external nodes of the form `Tip a` and internal nodes of the form `Node l b r`. The former are labelled with elements of type `a` while the latter are labelled with elements of type `b`. The type `Tree` has kind `* -> * -> *`, which is auto curried like all binary type ctors.

```hs
-- recursive data type
type Nat :: *
data Nat = Zero | Succ Nat

-- String as a recursive data type
type String :: *
data String = NilS | ConsS Char String

-- List by abstracting over Char (in String above)
type List :: * -> *
data List a = Nil | Cons a (List a)

-- (Tip a) v (Node l b r)
type Tree :: * -> * -> *
data Tree a b = Tip a | Node (Tree a b) b (Tree a b)

-- multiway branching tree
type Rose :: * -> *
data Rose a = Branch a [Rose a]

-- mutually recursive data types
type Rose :: * -> *
type Forest :: * -> *
data Rose a = Branch a (Forest a)
data Forest a = Nil | Cons (Rose a) (Forest a)

-- abstracting over type ctors
type GRose :: (* -> *) -> * -> *
type role GRose nominal nominal
data GRose f a = GBranch a (f (GRose f a))
```

The `Rose` data type captures multiway branching trees, aka *rose trees*. A node is labelled with an element of type `a` and has a list of subtrees. The type Rose falls back on the type `List`, but instead, we may introduce it using two mutually recursive data type declarations.

* In Haskell, the type parameters of a data type may range over type ctors of arbitrary kinds, *Higher-order Kinded data types*, unlike Miranda, Standard ML and even previous versions of Haskell (before 1.2) that only have *First-order Kinded data types*.

We can generalize `Rose` to abstract over the `List`. The type ctor `GRose` has *second-order kind*. Applying `GRose` to `List` yields the type of Rose trees.

To calculate the **order of a kind** use the formula:

```hs
--     Type ≡ *
order (Type)   = 0
order (k -> v) = max (1 + order k) (order v)
```

The following data type declaration introduces a fixed point operator on the level of types. This definition can be employed to give a generic definition of *catamorphisms* and *anamorphisms*. Using `Fix` and `ListBase` the data type of parametric lists can alternatively be defined by (3) below.

```hs
type role Fix nominal
type Fix :: (* -> *) -> *
newtype Fix f = In (f (Fix f))

type ListBase :: * -> * -> *
data ListBase a b = NilL | ConsL a b

-- 3
type List :: * -> *
type List a = Fix (ListBase a)
```

### Nested data type

A *regular* or *uniform data type* is a recursive, parameterized type whose definition does not involve a change of the type parameters.

The data types of the previous section are without exception regular types, but now we explore *non-regular* or *nested data types*. Nested data types are practically important since they can capture data-structural invariants in a way that regular data types cannot.

For instance, the following data type declaration defines perfectly balanced, binary leaf trees, *perfect trees* for short. This equation can be seen as a bottom-up definition of perfect trees: a perfect tree is either a singleton tree or a perfect tree that contains pairs of elements. Note that the height of the perfect tree is encoded in the prefix of `SuccP` and `ZeroP` constructors.

```hs
data Fork a = Fork a a

type Perfect :: * -> *
data Perfect a = ZeroP a | SuccP (Perfect (Fork a))
```

The next data type provides an alternative to the ubiquitous list type if an efficient indexing operation is required: Okasaki's *binary random-access lists* support logarithmic access to the elements of a list.

This definition captures the invariant that binary random-access lists are sequences of perfect trees stored in increasing order of height.

```hs
-- binary random-access lists
type Sequ :: * -> *
data Sequ a
  = EndS
  | ZeroS (Sequ (Fork a))
  | OneS a (Sequ (Fork a))
```

The `Perfect` and `Sequ` are examples of *linear nests*: the parameters of the recursive calls do not themselves contain occurrences of the defined type.


A *non-linear nest* is the following type: an element of type `Bush a` resembles an ordinary list except that the `i`-th element has type `Bushⁱ a` rather than `a`.

```hs
type Bush :: * -> *
data Bush a = NilB | ConsB a (Bush (Bush a))

x0 :: Bush Integer
x0 =               ConsB 1                      -- 1st element
           (ConsB (ConsB 2 NilB)                -- 2nd element
    (ConsB (ConsB (ConsB 3 NilB) NilB) NilB))   -- 3rd element
```


Haskell's data construct is surprisingly expressive; in fact, all primitive data types (such as characters or integers) can, in principle, be defined by a data declaration. *The only exceptions to this rule are the function space constructor and IO data type*.

## Towards generic programming

The basic idea of generic programming is to define a function (such as equality) by *induction on the structure of types*.

Thus, generic equality takes 3 args: a type and two values of that type, and proceeds by case analysis on the type argument. In other words, generic equality is a *function that depends on a type*.
