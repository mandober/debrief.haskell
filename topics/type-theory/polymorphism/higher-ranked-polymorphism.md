# Polymorphism

Types of polymorphism
- universal polymorphism
  - parametric polymorphism, compile-time polymorphism
  - subtype polymorphism, inclusion polymorphism, runtime polymorphism
- ad hoc polymorphism, nonuniversal polymorphism
  - overloading polymorphism
  - coercion polymorphism, casting polymorphism
- Higher-ranked polymorphism
  - predicative polymorphism
  - impredicative polymorphism
  - Rank-1 polymorphism prenex polymorphism, let-polymorphism
  - Rank-2 polymorphism
  - Rank-k polymorphism
  - Rank-n ("higher-rank") polymorphism


# Higher-ranked polymorphism

## Rank-1 (prenex) polymorphism, Let-polymorphism

> In a prenex polymorphic system, type variables may not be instantiated with polymorphic types.

(This is very similar to what is called "ML-style" or "Let-polymorphism", although, technically ML's Let-polymorphism has a few other syntactic restrictions as well)

This restriction makes the distinction between polymorphic and non-polymorphic types very important; thus, in *predicative systems* polymorphic types are sometimes referred to as **type schemas** to distinguish them from ordinary, *monomorphic types*, which are sometimes called *monotypes*.

A consequence is that all types can be written in a form that places all quantifiers at the outermost (prenex) position.

For example, consider the `append` function, which has type   
`forall a. [a] × [a] -> [a]`

```hs
append :: forall a. ([a], [a]) -> [a]

append :: forall a. [a] -> [a] -> [a]

-- predicative instantiation
> append @Int
append @Int :: [Int] -> [Int] -> [Int]

-- predicative instantiation
> append @(Int -> Int)
append @(Int -> Int) :: [Int -> Int] -> [Int -> Int] -> [Int -> Int]

-- impredicative instantiation
> append @(forall a. a -> Int)
```

In order to apply this function to a pair of lists, a type must be instantiated, substituting the type variable `a`, such that the type of the arguments matches up with the result type of `append` function.

In an impredicative system, the type being substituted may be any type whatsoever, including a type that is itself polymorphic; thus `append` can be applied to pairs of lists with elements of any type - even to lists of polymorphic functions such as `append` itself.

Polymorphism in the language ML is predicative. This is because predicativity, together with other restrictions, makes the type system simple enough that full type inference is always possible.

As a practical example, OCaml (a descendant or dialect of ML) performs type inference and supports **impredicative polymorphism**, but in some cases when impredicative polymorphism is used, the system's type inference is incomplete unless some explicit type annotations are provided by the programmer.


## Rank-k polymorphism

For some fixed value `k`, rank-k polymorphism is a system in which a quantifier may not appear to the left of `k` or more arrows (when the type is drawn as a tree).

Type inference for rank-2 polymorphism is decidable, but reconstruction for rank-3 and above is not.

## Rank-n polymorphism

**Rank-n polymorphism** or **higher-rank polymorphism** is polymorphism in which quantifiers may appear to the left of arbitrarily many arrows.


## Predicativity and impredicativity

## Predicative polymorphism

In a predicative parametric polymorphic system, a type 
\tau  containing a type variable 
\alpha  may not be used in such a way that 
\alpha  is instantiated to a polymorphic type. Predicative type theories include Martin-Löf type theory and NuPRL.

## Impredicative polymorphism

Impredicative polymorphism (also called first-class polymorphism) is the most powerful form of parametric polymorphism. A definition is said to be impredicative if it is self-referential; in type theory this allows the instantiation of a variable in a type 
\tau  with any type, including polymorphic types, such as 
\tau  itself. An example of this is the System F with the type variable X in the type 
T = \forall X. X \to X, where X could even refer to T itself.

In type theory, the most frequently studied impredicative typed λ-calculi are based on those of the lambda cube, especially System F.

## Bounded parametric polymorphism

Main article: Bounded quantification

In 1985, Luca Cardelli and Peter Wegner recognized the advantages of allowing bounds on the type parameters. Many operations require some knowledge of the data types, but can otherwise work parametrically. For example, to check whether an item is included in a list, we need to compare the items for equality. In Standard ML, type parameters of the form ''a are restricted so that the equality operation is available, thus the function would have the type ''a × ''a list → bool and ''a can only be a type with defined equality. In Haskell, bounding is achieved by requiring types to belong to a type class; thus the same function has the type 

{\scriptstyle Eq \, \alpha \, \Rightarrow \alpha \, \rightarrow \left[\alpha \right] \rightarrow Bool} in Haskell. In most object-oriented programming languages that support parametric polymorphism, parameters can be constrained to be subtypes of a given type (see Subtype polymorphism and the article on Generic programming).


---

* Polymorphism is a concept in PL theory. It means "many shapes", and the shapes here are types. Actually, types are more like casts, as they mold their terms; so, it is more that all the terms of particular type have a particular "shape" that the type imprinted on them.

* Many language entities can be polymorphic, but certainly this term comes most often when discussing functions.

* Polymorphism is about making a function (from now on all polymorphic language entities will be represented by considering functions as their representative) work with more than one type.

Dynamic languages can be consider polymorphic by default - all values in a dynamic PL are polymorphic, that is, you cannot specify types (e.g. in a function's signature) even if you wanted to. Any function will accepts a value of any types, and within the function the received value can be examined and cast between the types.

Static languages have a type system, which is active until after the compile-time. This, in turn, means that many IDEs will rely on the type system to inform you about the CT errors as you code, at design-time. However, there are type systems and then there are "type systems".

The `C` PL has the later; its has a very rudementary type system that can only prevent some elementary mistakes. On the other hand, C lack polymorphism, meaning the type system must be respected, which often is quite a nuissance. For example, the lack of polymorphism, but the presence of types, means your function that just adds the two inputs is typed to receive two `int`s and to return an `int`. For any other combination of numeric types, you have to write a dedicated function; in fact, you only have to write a dedicated *function header* by specifing the involved types, while the *function body* can remain pretty much identical for each *instance*. The term "instance" can be consider just that: virtually the same implementation (function body) with varying signature (function header).

This is exactly what polymorphism has made immensily convenient. Instead of a multitude, you only write one implementation (definition), but you specify the most general types, i.e. *type variables*, rather than some concrete ones.

* Every type system excludes some correct programs, and permits some incorrect ones. This is the famous gap between the capabilities of a type-checker and all the expressable but untypable things in a language. The reaserch of type system aims to bridge this gap, so that everything that can be expressed in a PL can also be type checked for correct behaviour.

For example, a language that lacks polymorphism will reject this program:

```hs
f :: [Int] -> [Bool] -> Int
f is bs = length is + length bs
```

because the `length` function cannot apply to both a list of Ints and a list of Bools. The solution is to use a more sophisticated type system in which we can give `length` a polymorphic type.

```hs
-- monomophic:
length :: [Int] -> Int
-- polymophic enough:
length :: [a] -> Int
-- utterly polymophic:
length :: (Traversable t, Num n) => t a -> n
```

Conversely, most languages will accept the expression `s + d`, where `s` is a variable representing speed, and `d` represents distance, even though adding a speed to a distance is nonsensical as adding a character to a boolean.

The type-system designer wants to accommodate more good programs and exclude more bad ones, without going overboard and losing the virtues mentioned above.
