# Algebraic data types

In Haskell, algebraic data types (ADTs) are just data types, but what makes them algebraic is their composition. The two main way to combine such types give us the product and coproduct types.

The product types form an algebra with product as a binary operation and the unit type as unit, `(T, ×, ())`.

The coproduct types form an algebra with coproduct as a binary operation and the `Void` type as unit, `(T, +, Void)`.

The canonical product type is a pair, `(a, b)`, however the product types are formed in type declarations, consisting of a type ctor and its parameters on the left-hand side, and a data ctor and its parameters, called the *fields*, on the right-hand side.

```hs
data P = D f₁ f₂ … fₙ
```

More concretely, the simpleset product type is the unit defined as

```hs
data Unit = Unit
```

It has the type ctor on the left, `Unit`, and, like all product types, it has a single data ctor, also called `Unit`, on the right. Being the simplest product type, it has no fields associated with the data ctor. A slightly less simple is the Identity type, which is a product type with one field:

```hs
data Identity a = Identity a
```

The only field associated with the `Identity` data ctor has a generic type `a`, and so the type parameter `a` must also be declared on the left side. An example with the concrete type (instead of a generic) would be:

```hs
data Km = Km Int
```

In fact, these 3 types introduced so far are not really product types as there is no product to talk about.

But what exactly are `Unit`, `Identity` and `Km`? Are they all isomorphic to the unit type, `()`? Well, `Unit` is isomorphic to `()`, since it also has a single value, but the `Identity` and `Km` have way more values. Yet they certainly are not coproducts, nor are they product types, are they? Maybe these two could be interpreted as half-products, as a degenerate case of product types, similarly to how `n`-tuples are genuine tuples only for n >= 2, but when n = 1 it suggest the existence of a 1-tuple... Which is what? A degenerate pair with a single component? But is it `(a,)` or `(,a)`? Can it be denoted at all? Hand at heart, Haskell has the type called `Solo` that represents exactly the type of 1-tuple. And in case when n = 0, we get the unit type, which is the empty tuple, justifying its denotation as `()`.

Where do `Identity` and `Km` fit then?

The types like `Identity` and `Km` must be then isomorphic to the `Solo` type. Because in the algebra of types, there are no types "in between". That is, in the algebra of types we have:
- `Void` representing zero (coproduct unit), uninhabited type
- `()` representing one (product unit), singleton type (has 1 value)
- `⨯` representing a product type constructor
- `+` representing a coproduct type constructor
- and other, higher-order type ctors, primarily `->` for exponential types

A product type is a perhaps best called a record, where a record is the entire left-hand side of the product's definition. That is, the right side where we state the name of the unique data ctor, and then list its fields.

```hs
data P a b … z = D a b … z

data P1        = D1 Int
data P2        = D2 Int String
data P3        = D3 Int String Bool
data P4 a      = D4 Int a Bool
data P5 a b    = D5 a Float b
data P6 a b    = D6 Float (a -> b)
data P7 a b    = D7 a (a -> b)
```

The right-hand side of a product's definition could be called a *record*. It consists of the unique *data ctor* name and has any number of *fields*. A field is a represented by a type. If the type of a field is generic, then we must declare that type varaible on the left-hand side, as a type paramter to the type ctor.

```hs
data Pair a b = Pair a b
```

The `Pair` above is a bona fide product type. What makes it a genuine product type is the fact that we can express its right hand side a pair:

```hs
-- (1) genuine product type
data Pair a b = Pair a b

-- (2.1) this type has only a single field!
newtype Pair a b = Pair (a, b)
-- (2.2) equivalent
newtype Pair a b = Pair ((,) a b)

-- (2.3) but we cannot define it is
data Pair a b = (,) a b
-- since that would cause the error: Illegal binding of an existing name
```

When a product type is declared using *fields*, like in (1), as opposed to placing its fields in a tuple, as in (2), definitions are equivalent, but it is more convenient to work with proper products (e.g. less parens in pattern matching).

In Haskell, the blank space seems to represent the product operator. In OCaml, for example, the `Pair` type above would be declared as:

```ocaml hs
type 'a 'b pair = pair of 'a * 'b
```

i.e. there is a real product, as a tuple/pair on the right hand side, which is perhaps closer to the mathematical notation `A ⨯ B`. 

In math, the type of a pair is denoted by `A ⨯ B`, but a value of this type is denoted by `(a, b)`. 

In Haskell, the type of a pair and a value of that type could both be denoted by `(a, b)`.

```hs
pairUp :: a -> b -> (a, b)
pairUp a b = (a, b)
```


On the other hand, coproduct types have stranger construction. First of all, in order to define a sum type, we must use the Haskell's built in syntax, i.e. the language construct `|`.

```hs
data C = D1 | D2
data C1 a b = D1 a | D2 b

data Maybe a = None | Just a
data Either e a = Left e | Right a
data List a = Nil | Cons a (List a)
```

The canononical coproduct type is `Either`, and, at least until it is defined, we must use the builtin syntax with the pipe symbol to define sum types.

>A sum type consists of *at least 2 records*, where a *record* is the right-hand side of a product type.

That is why the types in the algebra of types are also called *sums of products*. Actually, the full name is: **possibly recursive sums of products**.
