# Types, Data Abstraction and Polymorphism



`On Understanding Types, Data Abstraction, and Polymorphism`
paper (1985) Luca Cardelli, Peter Wegner

**Monomorphic programming languages** are conventional typed languages based on the idea that functions, hence their operands, have a *unique type*; such languages are said to be *monomorphic*, in the sense that every value and variable can be interpreted to be of one and only one type.

**Polymorphic programming languages** have some values and variables that may have more than one type. *Polymorphic functions* are functions whose operands (arguments) can have more than one type. *Polymorphic types* are types whose operations are applicable to values of more than one type.

In 1967, Strachey had distinguished between two major kinds of polymorphism:
- **Parametric polymorphism** is obtained when a function works uniformly on a range of types, which normally share a common structure (hmm...)
- **Ad-hoc polymorphism** is obtained when a function works on several different types (which may not share a common structure), and may behave in unrelated ways for each type.

Polymorphism
- universal polymorphism
  - parametric polymorphism
  - inclusion polymorphism
- nonuniversal (ad-hoc) polymorphism
  - overloading polymorphism
  - coercion polymorphism

[Luca Cardelli and Peter Wegner 1985]'s classification of polymorphism refines that of Strachey by introducing a new form of polymorphism called *inclusion polymorphism* to model subtypes and inheritance (to model OOP).

Parametric and inclusion polymorphism are classified as the two major subcategories of *universal polymorphism*, which is contrasted with *nonuniversal (ad-hoc) polymorphism*.

Parametric polymorphism is so called because the uniformity of type structure is normally achieved by type parameters, but uniformity can be achieved in different ways, and this more general concept is called universal polymorphism.

Universally polymorphic functions will normally work on an infinite number of types (all the types having a given common structure), while an ad-hoc polymorphic function will only work on a finite set of different and potentially unrelated types.

In the case of universal polymorphism, one can assert with confidence that some values, i.e. parametically polymorphic functions, indeed have many types, while in ad-hoc polymorphism this is more difficult to maintain, as one may take the position that *an ad-hoc polymorphic function is really a small set of monomorphic functions*.

> In terms of implementation, a universally polymorphic function will execute the same code for arguments of any admissible type, while an ad-hoc polymorphic function will execute different code for each type of argument.

**Monomorphisation** is a technic where a parametically polymorphic function is converted into a monomorphic function each time it is called with an argument of distinct type. For example, the `length` function which is truly parametically polymorphic function and could execute the same code for any type of argument, when called with an integer list, gets converted into a functions that works exclusively with a list of integers. 
The original function remains, but a copy that is specialized to work with integer list is created. If it gets called again with an integer list (in the same module), the specilized function is used, but if it gets called with another type, like a list of strings, then a copy specialized to lists of strings is created. The monomorphisation increases efficiency because each specialized version works with a single concrete type, so the properties of that type are exploited to build the most suitable implementation. Of course, the price for this is increase in the code size. 
Rust uses monomorphisation, Haskell doesn't.


There are two major kinds of universal polymorphism, i.e. two major ways in which a value can have many types.
- In *parametric polymorphism*, a polymorphic function has an implicit or explicit type parameter, which determines the type of the argument for each application of that function.
- In *inclusion polymorphism* an object can be viewed as belonging to many different classes which need not be disjoint, i.e. there may be inclusion of classes.

These two views of universal polymorphism are not unrelated, but are sufficiently distinct in theory and in practice to deserve different names.

The functions that exhibit parametric polymorphism are also called *generic functions*. For example, the `length` function from lists of arbitrary type to integers is called a generic length function. A generic function is one which can work for arguments of many types, generally doing the same kind of work independently of the argument type. If we consider a generic function as a single value, it has many functional types and is therefore polymorphic.

There are also two major kinds of *ad-hoc polymorphism*.
- In *overloading* the same variable name is used to denote different functions, and the context is used to decide which function is denoted by a particular instance of the name. We may imagine that a preprocessing of the program will eliminate overloading by giving different names to the different functions; in this sense overloading is just a convenient syntactic abbreviation.
- A *coercion* is instead a semantic operation which is needed to convert an argument to the type expected by a function, in a situation which would otherwise result in a type error. Coercions can be provided statically, by automatically inserting them between arguments and functions at compile time, or may have to be determined dynamically by run-time tests on the arguments.

*Value sharing*: the constant value `null` may is also polymorphic since it can stand for any type (or, at least, for all pointer types). This kind of value polymorphism is also found in Haskell, which has the `undefined` value whose type is polymorphic `undefined :: forall a. a` (it is the least defined type, aka the bottom, ⟘).

~ ~ ~

The first-order typed λ-calculus can be enriched with second-order features intended to model polymorphism and object-oriented languages.

A first-order language has a set of *base types* (Bool, Int, Real, String), and the means of constructing new types (functions, product types, sum types, recursive types). The set of first-order types is used as a base for introducing parametric types, abstract data types, and type inheritance by means of second-order language features.

Viewing types as sets allows us to define
- parametric polymorphism  in terms of *set intersection* of associated types
- data abstraction         in terms of *set union*        of associated types
- inheritance polymorphism in terms of *subsets*          of associated types

We can augment the first-order λ-calculus with
-   *universal quantification* for realizing *parameterized types*
- *existential quantification* for realizing *data abstraction*
-    _bounded quantification_  for realizing *type inheritance*

The syntactic extensions of the type expression sublanguage determined by these features may be summarized as follows:

```
Type := BaseType | QuantifiedType

QuantifiedType
  := ∀τ. Type                             Universal Quantification
   | ∃τ. Type                             Existential Quantification
   | ∀τ ⊆ Type. Type                      Bounded Quantification
   | ∃τ ⊆ Type. Type                      Bounded Quantification
```

**Universal quantification** enriches the first-order λ-calculus with *parameterized types* that may be *specialized* by substituting *actual type parameters* for *universally quantified parameters*. Universally quantified types are themselves first-class types and may be the arguments (actual parameters) in such a substitution.

**Existential quantification** enriches first-order features by allowing abstract data types with hidden representation.

Information hiding can be achieved not only through existential quantification but also through the *let construct*, which facilitates hiding of local variables in a module. Hiding by means of let is referred to as *first-order hiding* because it involves hiding of local identifiers and associated values, while hiding by means of existential quantifiers is referred to as *second-order hiding* because it involves hiding of *type representations*.

**Bounded quantification** enriches the first-order λ-calculus by providing explicit *subtype parameters*. *Inheritance* (i.e. *subtypes* and *supertypes*) is modeled by explicit parametric specialization of supertypes to the subtype for which the operations will actually be executed. In OO every type is potentially a supertype for subsequently defined subtypes and should therefore be modelled by a *bounded quantified type*. Bounded quantification provides an explanatory mechanism for *object-oriented polymorphism* that is cumbersome to use explicitly but useful in illuminating the relation between parametric and inherited polymorphism.

~ ~ ~

The typed λ-calculus can be augmented with various kinds of basic types. The base types may be:
- `Unit` is the trivial type, only a single element `()`
- `Bool`   with an if-then-else operation
- `Int`    with arithmetic and comparison operations
- `Real`   with arithmetic and comparison operations
- `String` with string concatenation

Structured types are built from the base types by means of type constructors:
- function spaces (->), function types
- Cartesian products (×), product types
- record types, *labeled Cartesian products*
- variant types, *labeled disjoint sums*, sum types


## Types are Sets of Values

What is an adequate notion of type which can account for polymorphism, abstraction and parametrization?

**There is a universe `V` of all values**, containing simple values like integers, data structures like pairs, records and variants, and functions.

**The universe `V` is a complete partial order** (relying on the Dana Scott's work in domain theory), but in the first approximation, we can think of it as just a large set of all possible computable values.

**A type is a set of elements of `V`**. However, not all subsets of `V` are legal types. Those subsets that have certain properties are called **ideals**. Hence, **a type is an ideal**, which is a set of values.

Moreover, *the set of all types (ideals) over V, when ordered by set inclusion, forms a lattice*. On the top of this lattice is the type `Top` (the set of all values, i.e. V itself). On the bottom of the lattice is the type `Bottom` (essentially, the empty set; but actually, it is the singleton set containing the *least element* of V).

The phrase "having a type" is then interpreted as membership in the appropriate set. As ideals over V may overlap, a value can have many types (?).

The set of types of any given PL is generally only a small subset of the set of all ideals over V. For example, any subset of the integers determines an ideal (and hence a type), and so does the set of all pairs with first element equal to 3. This generality is welcome, because it allows one to accommodate many different type systems in the same framework. One has to decide exactly which ideals are to be considered "interesting" in the context of a particular PL.

**A type system is a collection of ideals of V**, which is identified by giving a language of type expressions and a mapping from type expressions to ideals.

The ideals in this collection are elevated to the rank of types for a particular language. For example, we can choose the integers, integer pairs and integer-to-integer functions as our type system. Different languages have distinct type systems, but all these type systems can be built on top of the domain `V` using the same techniques.

In a **monomorphic type system** each value belongs to at most one type; that is, except for the least element of `V` which, by definition of ideal, belongs to all types.

As types are sets, a value may belong to many types. In a **polymorphic type system**, large and interesting collections of values belong to many types.

There is also a grey area of mostly monomorphic and almost polymorphic systems, so the definitions are left imprecise, but the important point is that the basic model of ideals over V can explain all these degrees of polymorphism.

Since types are sets, subtypes simply correspond to subsets. Moreover, the semantic assertion `T1` is a subtype of `T2` corresponds to the mathematical condition `T1 ⊆ T2` in the type lattice. This gives a very simple interpretation for subrange types and inheritance.

Finally, if we take our type system as consisting of the single set V, we have a type-free system in which all values have the same type. Hence we can express typed and untyped languages in the same semantic domain, and compare them.

The type lattice contains many more points than can be named in any type language. In fact it includes an uncountable number of points, since it includes every subset of the integers. The objective of a language for talking about types is to allow the programmer to name those types that correspond to interesting kinds of behavior.

To do this, the language contains *type constructors*, including *function type constructors* for constructing a *function type* `T :: T1 → T2`  from domain type `T1` and range type `T2`.

The type constructors allow an unbound number of interesting types to be constructed from a finite set of primitive types. However, there may be useful types of the type lattice that cannot be denoted using these type constructors.

There are more powerful type constructors that allow us construct types corresponding to infinite unions and intersections in the type lattice. In particular, **universal quantification** will allow us to name types whose lattice points are *infinite intersections of types*, while **existential quantification** will allow us to name types corresponding to *infinite unions*.

The model of ideals is not the only model of types which has been studied. With respect to other denotational models, however, it has the advantage of explaining simple and polymorphic types in an intuitive way, namely as sets of values, allowing a natural treatment of inheritance. Less satisfactory is its treatment of type parametrization, which is rather indirect since types cannot be values, and its treatment of type operators, which involves getting out of the model and considering functions over ideals. In view of this intuitive appeal, we have chosen the ideal model as our underlying view of types, but much of our discussion could be carried over, and sometimes even improved, if we chose to refer to other models.

The idea of types as parameters is fully developed in the second-order λ-calculus. The (only known) denotational models the second-order λ-calculus are retract models (Scott). Here, types are not sets of objects but special functions called **retracts**; these can be interpreted as identifying sets of objects, but are objects themselves. Because of the property that types are objects, retract models can more naturally explain explicit type parameters, while ideal models can more naturally explain implicit type parameters.


## Universal Quantification and Generic Functions

The typed λ-calculus is sufficient to express monomorphic functions, however it cannot adequately model polymorphic functions. We cannot express the idea of a functional form that is the same for a variety of types, and we must explicitly bind variables and values to a specific type at a time when such binding may be premature.

The fact that a given functional form is the same for all types may be expressed by universal quantification. In particular, the identity function may be expressed as `id :: forall a. a -> a`. In the definition, `a` is a type variable and the `forall a.` part provides type abstraction for `a`. To apply it, we must first supply a type argument and then a value argument of that type: `id @Int 4` (this is not needed in Haskell since it can just infer the appropriate type from the argument, but it is possible).

We refer to functions, such as `id` (that require passing in both type and value parameters) as *generic functions*.

```hs
{-# LANGUAGE RankNTypes #-}

-- function definition
id :: forall a. a -> a  -- actually: forall a. (a -> a)
id = λx -> x

-- function application
n = id @Int 42
```

The `forall` or `∀` is a binding operator for types, just like `λ` is a binding operator for terms. They both require the matching arguments to be supplied during function application. However, `forall a` serves to bind a type, while `λx` serves to bind a variable of that (possibly generic) type.

ML and Haskell have a type inference mechanism that allows the system to infer the types of both monomorphic and polymorphic expressions, so that type specifications omitted by the programmer can be reintroduced by the system. The above can also be written by eliding the universal quantification in the definition, and type application in the call.

```hs
-- function definition
id :: a -> a
id = λx -> x

-- function application
n = id 42
```

Here is an example of a function with a universally quantified type parameter. When the function `idFactory` is applied only to a type argument, it gets specialized to that type. For example, when the type `Int` is passed in, we get back the identity function on integers, `Int -> Int`; if we pass in a `Bool` type arg, we get back the identity function on Booleans, `Bool -> Bool`.

```hs
idFactory :: (∀a. a -> a)
idFactory f = f

-- instantiate the identity function on Ints
idInt :: Int -> Int
idInt = idFactory @Int

-- instantiate the identity function on Booleans
idBool :: Bool -> Bool
idBool = idFactory @Bool
```
