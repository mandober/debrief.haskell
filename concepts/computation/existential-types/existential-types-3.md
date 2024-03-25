# Existential types in Haskell

>Existential types are ADTs that correspond to data encapsulation found in OOP.

Existential types are a way of hiding a type within a type. Hiding it in such a way that any consumer of the data type has no knowledge of the internal type, except for its relations encoded in the ADT. This is what allows ADTs in Haskell to be composed into *abstract data types* thus giving us the benefits of *modular programming*.

https://en.wikipedia.org/wiki/Encapsulation_%28computer_programming%29
https://en.wikipedia.org/wiki/Abstract_data_type
https://en.wikipedia.org/wiki/Modular_programming

>Only the *producer* knows exactly what the existential type is.

Producers and consumers can be thought to correspond to *type introduction* and *type elimination*, respectively.

In OOP terms, this means a producer is the scope that constructed the data type. The consumer isn't the destructor as it doesn't necessarily destroy the OOP object, but instead is the scope that inspects or manipulates the internal properties of the OOP object.

### Existential quantification in logic

Firstly, a *quantifier* is a construct that specifies the quantity of members that a variable ranges over in the domain of a predicate.

For example, in a predicate "X is a man", where X is a variable that inhabits a particular domain of discourse (i.e. the type of the variable), and X has to be quantified in some way. This predicate can be interpreted as a unary function with type `Animal -> Bool`. A function in this sense, is a *predicate* in logic programming languages.

To quantify X, we can either universally or existentially quantify the variable. To *universally quantify* it, means to say `is a man` is a property that holds for all members of the type `Animal`. To *existentially quantify* it means to say it only holds for some members (at least one) of the type `Animal`.

We can denote these two forms by the formulas

- `∀x(Animal x ⇒ Man x)`   
  for all `x` that are members of the `Animal` set, `x` is a man; 
  i.e. if `x` is an `Animal`, then `x` is a `Man` (dependent properties)

- `∃x(Animal x ∧ Man x)`   
  for some `x` that are in `Animal` set, `x` are in `Man` set; 
  i.e. `x` is an `Animal` and `x` is a `Man` (orthogonal properties)


Every predicate expressed in a natural language is always quantified, although quantification is often implicit relying on the conversational context.

For example, "the glasses in my recent order were chipped" vs "every glass in my recent order was chipped" and "some glasses in my recent order were chipped".

## Back to Haskell

Now to relate it back to Haskell and programming in general.

Haskell actually contains two languages:
- the term-level language
- the type-level language

Type-level programming in Haskell resembles a restricted form of logic programming.

At the term level, the term variables are implicitly universally quantified in their type domain for total functions (from the perspective of the Haskell's compiler, it cannot differentiate a partial function from a total function).

For example, the function

```hs
add1 :: Nat -> Nat -> Nat
add1 a b = a + b
```

has term variables `a` and `b`, and we can pass in any member of `Nat`.

*Partial functions* do not play nicely with the referentially transparent functional programming, since they lead to exceptions, which are side effects.

Working with partial functions, the type system no longer helps us.

I'm not sure of the relation between existential quantification and term level partial pattern matching.

As a side note, the PL `Mercury` actually disallows *partial pattern matching* in the predicate constructs, as it performs totality checking, but has other complications regarding co/recursion.

At the type level, type variables are by default universally quantified in order to allow more convenient expressions of parametric polymorphism.

The domain of discourse are types of kind `*`, or the category `Hask`. 

The `id` function is parametrically polymorphic; the type param `a` is universally quantified so it ranges over all types.

```hs
id :: forall a. (a -> a)
```

>What about existential quantification at the type level?

The `forall` syntax available in `ExplicitForAll` extension is also used with existential types. In fact, the `forall` keyword is used for 4 things:
- `ExplicitForAll`, explicit universal quantification
- `ExistentialQuantification`, existential quantification
- `ScopedTypeVariables`, extending the scope of type param over entire function
- `RankNTypes`, for Higher Rank Polymorphism


With `ExistentialQuantification` pragma enabled, we can use `forall` in ADT declarations, the basic syntax of which is:

```hs
data T = forall a. MkT a
```

`T` is an abstract type that encapsulates the existential type `a`.

`MkT` is the data ctor that takes an universally quantified type variable `a`. But since the type param `a` only appears on the rhs, it is actually existentially quantified, thus allowing `MkT` to take a value of any type.

The producer of a value of type `T` can choose any type to be encapsulated into `T` before passing it to a consumer. In OOP parlance, the `a` type will have a private scope with respect to the consumer.

If we were to check the type signature of the `MkT` constructor function, we would see something like this:

```
MkT :: forall a. a -> T
```

Which turns out to be equivalent semantically to this pseudo Haskell code that had the `exists` keyword implemented:

```
MkT :: (exists a. a) -> T
```

This holds true in general as long as the existential type lies only on the left side of the implication (function arrow `->`). There's a proof of this here: http://stackoverflow.com/a/10753957/582917 and I copied the relevant section:

> And (∀x. Q(x) ⇒ P)  =  (∃x. Q(x)) ⇒ P (this one is used below):
> 
> 1. (∀x. Q(x) ⇒ P)
> 2. (∀x. ¬Q(x) ∨ P)
> 3. (¬¬∀x. ¬Q(x)) ∨ P
> 4. (¬∃x. Q(x)) ∨ P
> 5. (∃x. Q(x)) ⇒ P
> 
> http://stackoverflow.com/a/10753957/582917

I reckon it would have been easier for beginners to understand, if instead this syntax was available from `ExistentialQuantification` extension:

```
data T = MkT (exists a. a)
```

Reading this http://stackoverflow.com/a/28549312/582917 shows that there's a limitation here. When the existential type is not on the left side of the implication, but on the right side, there's no way to directly express such a type as below:

```
g :: exists a. (a, a -> Int)
g = (2 :: Int, \x -> x+3)
```

Instead you need to wrap the above type into an abstract type, thus necessitating something like:

```
data T = forall a. MkT (a, a -> Int)
MkT :: forall a. (a, a -> Int) -> T
```

There is a better way to express existentials and data abstraction. It's the usage of GADT (Generalised Algebraic Data Types) syntax. It's provided by the `GADTSyntax` and `GADTs` extension.

```
data T = forall a. MkT a
```

Becomes:

```
data T where
    MkT :: a -> T
```

We can also drop the `forall` as we now know universal quantification is implicit. This syntax is quite nice when you see something like this:

```
data Compare a = forall b. Ord b => ASC (a -> b) | forall b. Ord b => DESC (a -> b)
```

Can be changed to:

```
data Compare a where
    ASC  :: Ord b => (a -> b) -> Compare a
    DESC :: Ord b => (a -> b) -> Compare a
```

Now these articles make more sense:

* http://etymon.blogspot.com.au/2006/04/what-is-object-oriented-programming.html
* http://iveselov.info/posts/2012-08-30-existential-types.html
* https://en.wikipedia.org/wiki/Abstract_data_type
* https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types#Example:_heterogeneous_lists

Once you have an abstract opaque data type that has internal existential types. You can only operate on those internal values based what information you have about those internal values. If they are existentially quantified with no other constraints, the only we know is that is some Haskell type. This means the only thing we can do is apply operations that work on all types such as the `id` function. When you combine existential types with type class constraints, we suddenly gain information about the interface of that type and what operations it supports. Beyond type classes is (what I call a) relationship specification, something like `(a, a -> Int)` doesn't tell us what `a` is, but it does tell us that the second member of the tuple is a function that can operate on whatever `a` is.
