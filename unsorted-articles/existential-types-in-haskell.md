Existential Types in Haskell
============================

I had always been confused by Haskell's implementation of existential types. 
Until now!

Existential types is the algebraic data type (ADT) equivalent to OOP's data 
[encapsulation](https://en.wikipedia.org/wiki/Encapsulation_%28computer_programming%29). 
It's a way of hiding a type within a type. Hiding it in such a way that any 
consumer of the data type won't have any knowledge on the internal property 
type, except the relationships encoded in the ADT. This is what allows algebraic 
data types in Haskell to be composed into [abstract data types](https://en.wikipedia.org/wiki/Abstract_data_type), thus giving us the benefits of [modular programming](https://en.wikipedia.org/wiki/Modular_programming).

Only the producer knows exactly what the type is. Note that producer and 
consumer can be thought of as introduction and elimination. In OOP terms this 
means a producer is the scope that constructed the data type. The consumer isn't 
the destructor as it doesn't necessarily destroy the OOP object, but instead 
is the scope that inspects or manipulates the internal properties of the OOP 
object.

Just to recap on what existential quantification is in logical terms. Firstly 
"quantifier" means a construct that specifies the quantity of members for a variable in the domain of a predicate. An example is a predicate `X is a man` 
where `X` is a variable that inhabits a particular domain of discourse (read this as the type of the variable), and `X` 
has to be quantified in some way. This expression can be interpreted as a single parameter function with the type signature `Animal -> Boolean`, which of course returns true or false. A function in this sense, is a "predicate" in logic programming languages. Now to quantify `X`, we can either choose to 
universally or existentially quantify the variable. To universally quantify it 
means to say `is a man` is a property that holds for all members of type `Animal`, while existential quantification would mean that it only holds for some members 
of type `Animal` where "some" could mean just one. We would express these 2 forms as:

```
∀x∈Animal IsAMan(x)
∃x∈Animal IsAMan(x)

or

For all x that is a member of the Animal set, such x is a man.
For some x that is a member of the Animal set, such x is a man.
```

Every predicate that we express in natural languages always has a quantification, but sometimes the quantification is implicit relying on the conversational context. For example: "the glasses in my recent order was chipped" versus "every glass in my recent order was chipped" and "some glasses in my recent order was chipped".

Now to relate it back to Haskell and programming in general. In Haskell, by default there are 2 languages. The term level language and the type level language. Note that the type level language in Haskell resembles a limited logic programming language.

At the term level, the term variables are always implicitly universally quantified in their type domain for total functions (from the perspective of the Haskell's compiler, it cannot differentiate a partial function from a total function). For example, this function `add1 :: Nat -> Nat -> Nat; add1 a b = a + b`, has term variables of `a` and `b`, you can always pass in any member of `Nat` with no problems. There is the case of partial functions, but it doesn't fit into the overall narrative of pure referentially transparent functional programming, as they will lead to exceptions which are handled as side effects. When working with partial functions, you're no longer working with the type system, and the compiler will no longer help you. So I'm not sure of the relationship between existential quantification and term level partial pattern matching. As a side note, Mercury actually disallows partial pattern matching in its predicate constructs, as it performs totality checking, and there are also other total function programming languages preventing partial functions, but has other complications relating to recursion/corecursion.

At the type level, type variables are by default universally quantified in order to allow more convenient expressions of parametric polymorphism. The domain of discourse is not the type anymore, but is the category Hask, which is the category of Haskell types and functions, or it could be the kind `*`, the kind of Haskell types. So `id :: a -> a` actually parametrically polymorphic, and `a` is a type variable universally quantified to encompass all possible types. More verbosely expressed as: `id :: forall a. a -> a`. You cannot write the second form by default, but you can with `ExplicitForAll`. But what about existential quantification at the type level?

Here's where we actually dig into existential types. Firstly we mentioned above the `forall` syntax available in `ExplicitForAll` extension. One of the major confusing things about this syntax is that it's used for 4 different things, and the Haskell implementors didn't add extra syntax to differentiate the 4. The 4 things are (in the format of feature - extension):

* Universal quantification - `ExplicitForAll`
* Existential quantification - `ExistentialQuantification`
* Extending the scope of a type variable over the entire function - `ScopedTypeVariables`
* Higher Rank Polymorphism - `RankNTypes`

There is a logic to the madness however. But I'm going to discuss universal and existential quantification here.

We've already shown universal quantification, and since it is the default case, there's nothing more to talk about. For existential quantification we need the `ExistentialQuantification` extension. This allows one to use `forall` in `data` declarations. The basic syntax for expressing it is:

```
data T = forall a. MkT a
```

Here `T` is a super abstract type that encapsulates an existential type of `a`. `MkT` is the constructor function of `T`. It takes a explicit universally quantified type variable `a`, thus allowing MkT constructor function to take in a value of any type. The producer of a value of type `T` can choose any type to be encapsulated into `T` before passing it to a consumer. In OOP parlance, the `a` type will have a private scope with respect to the consumer.

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