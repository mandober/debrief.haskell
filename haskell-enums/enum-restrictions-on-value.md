# List of restrictions

Restrictions on a value:
- type
  - its type
  - its type/kind level: non/primitive, un/boxed, un/lifted
- type paramater
  - type instantiation
  - the type param to instantiate
  - phantom type
- kindedness
  - its type's kind? kindedness
  - higher-kindedness of its type
- polymorphism
  - ad hoc polymorphism
  - parametric polymorphism
  - insufficient polymorphism, e.g. "the value isn't polymorphic enough"
  - levity polymorphism
- qualifiers
  - forall qualifier
  - universal type
  - existential type
- class
  - effective type class
  - class constraints
  - superclass constraints


## Any type

The phrase "takes any type" (`∀t`) has a high probability of being mentioned when discussing polymorphic functions, but the chances go through the roof in the case of the `id` function, that is the exemplar of such functions.

However, any such statement, with the details and relevant intricacies sufficiently exposed, most frequently evaluates to false. This may be justified by explaining that the abbreviated form of the phrase was meant, i.e. that the full version goes "all types of kind `Type`".

In any case, how would we go about declaring a function that really does accept a value of "any type at all" or `∀!t`? Or, equivalently, a datatype that works with ant type? Hmm, isn't `Proxy` a type like that? What would be the diff, if any, between a datatype vs a function with such a feature? What are the exact "extras" that need to be considered as one keeps abstracting, e.g. going from "all type" in the narrow sense, then abstracting over types, then kinds, them, ranks, levity, and so on, before reaching the ultimate "FOR ALL TYPES" level.

Its term parameter will surely be represented by a type parameter in its type signature. And this type param will certainly have to be full-on polymorphic and sufficiently so, a very high-ranking TP. It must also be all sorts of high: higher-type, higher-kinded, higher-ranked. Completely unconstrained. But how about the levity polymorphism, i.e. the ability of a function to abstract over liftedness of types (kinds?)? It must work with non/primitive, un/lifted, un/boxed, base/compound/complicated, and every other conceivable value. And, as of lately, it must also be linearly flexible.

## Abstraction levels (ranging over levels)


- a certain value of a certain type: no ranging over anything, `'a'`, which is of type `Char`, `'a' :: Char`
- a value ranging over a certain type, `x :: Int`

A variable `x`:
- has value `'a'`; so it doesn't range over `Char`, it just is a `Char`
- has unknown value of type `Char`: yes, now it ranges over `Char`

The value of variable `x`
- may be of some concrete (known) type
  - `x :: Int; x = 42` so `x = 42 :: Int`
  - `x = 'Z'` thus `x :: Char` or `x = 'Z' :: Char`
- may range over a concrete (known) type
  - that `x` ranges over the `Int` means `x :: Int` as well
  - but its value is unknown (at least at the moment)
  - type that `x` ranges over must/should be specified
- may range over a set of numeric types
  - `Integral` class is a union of Integer, Int{2ⁱ}, Word{2ⁱ}, Natural
  - `Floating` class includes `Float`, `Double`
