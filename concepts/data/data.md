# Data

Data, values, types, functions, arity, containers, functors, …

## Values

Having a bare, immediate value, like `x`, e.g. like `5`. 
The `Int` type, as a type of a value, makes the value "readily available". 
All base (aka "flat") types make their values readily available. 

How about compound types? 

The simplest compound type should be "compound" in the simplest way, just enough to be able to hold onto a given value. Lists are "too compound", but in case of singleton lists, they give the idea of "bare compoundness". A singleton list holding a single value 5 is literally [5], and its type is [Int], which may also be denoted by `[] 5`, or more clearly as`List 5`. This suggest a form such type sould have - a unary type ctor, definitely, but what about the right side? Well, the same, only a unary data ctor:

```hs
data Identity a = Identity a

-- now we can give it a value to store
x :: Identity Int
x = Identity 5
```

The type `Identity` can hold a (single) value of any type `a`, but what about the other (compound) types?

`Maybe a` is similar, but until we examine it we cannot know if it indeed holds a value or not. `IO a` does holds a value of type `a` but we can never extract it - only work with it while inside the `IO` monad. `List a` has the same form, only it can hold multiple values of type `a` - or none at all.

All this types mentioned can indeed hold onto a value - well, in case of lists and `Maybe`, there may or there may not be a value, but when we do give them a value explicitly - they can sure hold onto that value (lists and Maybe as well).

Then we have types that can potentially store two values - types like tuples, `Either`, and functions - but functions, of all the other types, do make an instersting case. A function - considered in its default, unary, generic form - as a function from `a` to `b` is rather peculiar: it does not really hold onto anything. It is more like a trasformer of (potential) values, not a storage of values. Having a function `f : a -> b` lets us convert a value of type `a` into a value of type `b`, but for that we have to first have a value of type `a`. In a way it is almost as good as having a value of type `b`; as if a function `f : a -> b` (as a whole) is somehow isomorphic to `b`.

Functions as storage

However, we can force functions store values by considering that they are really closures - and closures can capture values from their environment.

To make a function store a single value, we already have the appropriate function, `const`. We just give it a value to store, `x`, which we can retrive later by calling "it" with any argument. The "it" here is the partially applied `const` function:

```hs
-- partially apply `const` to a value we want to store
fx :: a -> Int
fx = const 5

-- call the (const x) with any arg to retrive x
x1 :: Int
x1 = fx ()
```

Thus, `const x y` is isomorphic to `x`:
>const x y ≅ x

Or, `const x _` to make it more explicit that the second arg is irrelevant. That is its value is irrelevant, but it must exists - we must apply the function to something in ordered to retrieve the stored value. That "something" is usually the value that canonically signifies existence, `()`.

Storing two values

We can extend this scheme to store two values. We need a ternary (in Haskell's sense) function so it takes two payload args, leaving the final arg to be called later when we need to retrieve either. We store the two values by partially applying the function (intended for this purpose); which then awaits for the final argument. That final arg will be a function that lets us select which value we would like to retrieve; we cannot retrive both at once (well, we can, using a pair, but we are pretending no pairs exists yet, staying close to lambda calculus).

```hs
store2 :: a -> b -> (a -> b -> c) -> c
store2 a b s = s a b

store2P :: a -> b -> (a -> b -> _) -> (a, b)
store2P a b s = s a b

store2E :: a -> b -> (a -> b -> _) -> Either a b
store2E a b s = s a b
```

The `store2` above is exactly the *Vireo bird*, aka the *V combinator*, from lambda calculus, `V := λabc.cab`, also called "pairing", and in combinatory logic expressed as `BCT` (B is compose; C is flip; T is Thrush, λab.ba aka CI).
