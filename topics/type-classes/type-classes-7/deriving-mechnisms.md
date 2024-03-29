# Haskell concepts :: All deriving mechnisms

## Contents
- compiler auto-deriving of the standard classes
- deriving clause attached to a type declaration
- standalone deriving clause
- blanked deriving


## TOC
<!-- TOC -->

- [Contents](#contents)
- [TOC](#toc)
- [Intro](#intro)
- [Blank implementation](#blank-implementation)
- [Classes and instances](#classes-and-instances)
    - [Haskell Reports[🔗][5]](#haskell-reports🔗5)
    - [Countable sets[🔗][6]](#countable-sets🔗6)
- [Motivation[🔗][7]](#motivation🔗7)
- [Deriving[🔗][8]](#deriving🔗8)
- [Strategies[🔗][13]](#strategies🔗13)
- [Standard deriving[🔗][15]](#standard-deriving🔗15)
- [Auto derived[🔗][19]](#auto-derived🔗19)
  - [Typeable[🔗][20]](#typeable🔗20)
  - [Coercible[🔗][23]](#coercible🔗23)
  - [HasField[🔗][25]](#hasfield🔗25)
- [Derive Whatever[🔗][28]](#derive-whatever🔗28)
  - [Functor[🔗][29]](#functor🔗29)
  - [Foldable[🔗][30]](#foldable🔗30)
  - [Traversable[🔗][31]](#traversable🔗31)
  - [Generic and Generic1[🔗][32]](#generic-and-generic1🔗32)
  - [Data[🔗][33]](#data🔗33)
  - [Lift[🔗][34]](#lift🔗34)
- [Newtypes[🔗][36]](#newtypes🔗36)
- [Any class derivations[🔗][39]](#any-class-derivations🔗39)
  - [Generic anyclass[🔗][41]](#generic-anyclass🔗41)
  - [Exception anyclass[🔗][43]](#exception-anyclass🔗43)
  - [Anyclass ambiguity[🔗][44]](#anyclass-ambiguity🔗44)
- [Via[🔗][45]](#via🔗45)
- [Standalone deriving[🔗][47]](#standalone-deriving🔗47)
- [Empty Deriving[🔗][50]](#empty-deriving🔗50)
- [Best practices with Deriving[🔗][52]](#best-practices-with-deriving🔗52)
- [Summary[🔗][60]](#summary🔗60)
- [Quiz: Lock, Stock and Two Smoking Barrels[🔗][61]](#quiz-lock-stock-and-two-smoking-barrels🔗61)
  - [Training 1: Specify strategy[🔗][62]](#training-1-specify-strategy🔗62)
  - [Training 2: Disambiguate[🔗][63]](#training-2-disambiguate🔗63)
  - [Puzzle 1: Semigroup and Monoid[🔗][64]](#puzzle-1-semigroup-and-monoid🔗64)
  - [Puzzle 2: Infinite deriving[🔗][66]](#puzzle-2-infinite-deriving🔗66)
- [Conclusion[🔗][67]](#conclusion🔗67)
- [Sources[🔗][68]](#sources🔗68)

<!-- /TOC -->

## Intro

Out of the box (even supported by Haskell 98 and 2010), GHC provides a deriving mechanism that can replace manual, error-prone labor with automated code generation mechanism that auto-derives instances of many standard classes including
- Show
- Read
- Eq
- Ord
- Bound (restrictions apply)
- Enum (restrictions apply)
- Num (restrictions apply)

Thsi feature is so successful and practical it was immediately stolen by the rust compiler to auto-derive traits (lesser type classes).

Automatic deriving is used by attaching one or more *deriving clauses* to a type declaration.

```hs
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Ord)
  deriving (Show, Read)

instance deriving Functor List
instance deriving (Show a) => Show (List a)

newtype Zipper a = Zipper [(Tree a, Dir a (Tree a))]
  deriving newtype Eq
  deriving stock Ord
```

## Blank implementation

If a class has default implementation for required methods, an instance declaration can optionally skip the `where` keyword.

```hs
-- 'where' clause is optional
instance TreeLike Tree where
instance TreeLike Tree
-- vs standalone deriving (without the 'where' clause)
deriving instance TreeLike Tree
```


---


And here is a math typeclass-instance reference for math lovers: sets of numbers (naturals, rationals, etc.) and set properties. For example, math defines an object called *countable set* that has an infinite number of elements. Using this properly you can prove different theorems or deduce new properties. You don't even need to have a real object satisfying this property! But if you have such an object, you get all the proven theorems and properties automatically for your object.


Let's look at one code example for the better illustration of the instance-typeclass relationship. We can define a typeclass that would tell us whether you were a nice or naughty child this year

```
data Behaviour
    = Naughty
    | Nice

class YearBehaviour a where
    yearBehaviour :: a -> Behaviour
```

And that `yearBehaviour` method could be used with a lot of data types: `Int`, `Double`,… name them all! Let's have our first instances to show how it works:

```
instance YearBehaviour Int where
    yearBehaviour :: Int -> Behaviour
    yearBehaviour 0 = Naughty
    yearBehaviour _ = Nice

instance YearBehaviour Double where
    yearBehaviour :: Double -> Behaviour
    yearBehaviour n
        | isNaN n || isInfinite n = Naughty
        | otherwise = Nice
```

And then you can write polymorphic functions and not worry about which specific type is underhood until it has the instance of the desired typeclass:

```
canHaveChristmasGift :: YearBehaviour a => a -> Text
canHaveChristmasGift x = case yearBehaviour x of
    Nice    -> "Ho-ho-ho! Looks like somebody deserves a toy!"
    Naughty -> "You were naughty this year! Better luck next year ;)"
```

This is how it works in action:

```
ghci> canHaveChristmasGift (42 :: Int)
"Ho-ho-ho! Looks like somebody deserves a toy!"
ghci> canHaveChristmasGift (0 / 0 :: Double)
"You were naughty this year! Better luck next year ;)"
```

However, if we try to use this function with something that doesn't implement any instance of our typeclass, we will get the corresponding compiler error, that would warn us exactly about that:

```
ghci> canHaveChristmasGift ("Trust me, I am nice!" :: Text)

<interactive>:21:1: error:
    • No instance for (YearBehaviour Text)
        arising from a use of 'canHaveChristmasGift'
    • In the expression: canHaveChristmasGift "Trust me, I am nice!"
      In an equation for 'it': it = canHaveChristmasGift "Trust me, I am nice!"
```

Now when we know the above information about type classes and how they work, we may look at what deriving gives us and how it is connected.

## Motivation[🔗][7]

Without further ado, **deriving** is the mechanism of automatically generating typeclass instances for data types by a compiler.

The fair question here is why one may need to generate instances automatically? Let's try to get an answer to this one.

For instance, we have the gift system for Santa's *"Christmas"* operation, and we define the following data types in our system:

```
data Gift = Gift
    { giftId   :: Int
    , giftType :: GiftType
    }

data GiftType
    = Candies CandyCounter
    | Toy Name

newtype Name = Name
    { unName :: Text
    }

newtype CandyCounter = CandyCounter
    { unCandyCounter :: Int
    }
```

And now we want to compare two gifts - two values of the `Gift` data type (e.g., we may need it to prepare the most deserved presents depending on the child's behaviour).

We can, of course, write the special function `compareGifts :: Gift -> Gift -> Ordering`. But, generally, it's a good practice to provide a more polymorphic interface when possible. And here it is definitely possible!

One can notice that our data types would benefit from instances of the general comparison typeclass `Ord`, which already includes many comparison functions and the ecosystem nicely integrates with it. So, let's go ahead and write some instances.

If we look at the `Ord` typeclass definition, we can see that it demands the data type to have the `Eq` instance too. That means we should implement it first.

We also can verify in the docs, that the standard type `Int` already has both `Eq` and `Ord` instances. At least we don't need to define instances for primitive data types, phew! However, the newtype that we defined manually is not the same as `Int` even though it has the same runtime representation (because it is a newtype). For each newtype introduced we need to somehow define the instance too.

So, taking all of that under consideration, to solve our problem, we must write:

-   `Eq` instance for the `CandyCounter` newtype
-   `Ord` instance for the `CandyCounter` newtype
-   `Eq` instance for the `Name` newtype
-   `Ord` instance for the `Name` newtype
-   `Eq` instance for the `GiftType` data type
-   `Ord` instance for the `GiftType` data type
-   `Eq` instance for the `Gift` data type
-   `Ord` instance for the `Gift` data type

And only after that, we will be able to use the `compare` function with gifts.

Writing all those instances by hand is quite tedious work. Besides, such work creates more work - a lot of code to maintain for the future you and your colleagues. Imagine that you need to go through the above cycle again when you slightly change anything in your data types. What a nightmare that could be!

Besides boredom, it could bring pain and lead to some dummy errors like this one:

```
instance Eq MyChangedType where
    MyChangedType a1 b1 (Xxx c1 c2) == MyChangedType a2 b2 (Xxx d1 d2) =
           a1 == a2
        && b1 == b2
        && c1 == c2
        && d1 == d2
```

Can you see what is wrong with this instance? Indeed, you can notice the mistake after looking at the instance for a while, but this is just an example. It could be a bit more tricky to notice in real-life code, so you should be extra careful in manual instance declaration and maintenance.

So, with all that in mind, we are coming to the interesting part, the part why we are all here. Here comes the deriving!

## Deriving[🔗][8]

As we already said, the **deriving** mechanism is the compiler feature that automatically generates the instances of some typeclasses for you. There are different ways of deriving, but the general idea is described by the Haskell language report, which implies that any Haskell compiler should have this feature out of the box.

Citing the Haskell Report 2010:

A derived instance is an instance declaration that is generated automatically in conjunction with a data or newtype declaration. The body of a derived instance declaration is derived syntactically from the definition of the associated type.

Haskell 2010 Report

This is a very accurate definition suitable for any kind of deriving. So, it should give you an idea of what is deriving in general.

> ☝️ As the deriving mechanism is the part of Haskell language specification, the implementation depends on the compiler. We are going to talk only about Glasgow Haskell Compiler (or GHC) in this post.

There are in fact a few ways to utilize the deriving feature in GHC:

1.  The `deriving` clause within the data type declaration.
2.  Standalone deriving anywhere in the code (needs a specific extension enabled).

The first one is the most common way of deriving, so we will focus on this approach for now. But we are going to see how standalone deriving works in a separate section as well, so stay tuned!

The syntax for the simple deriving clause is the following:

```
(data|newtype) Data = Data Declaration
    deriving (TypeClasses, Separated, By Comma)
```

Compiler parses such clauses and resolves them during the type checker stage. If there are no type errors or deriving errors, then the instances are created during the compilation.

Perfect, since we have an idea of deriving, let's try to solve the boring problem from the previous example by using the easiest way of deriving in Haskell. For that, we can add the `deriving` clause (also known as *Deer-iving Claus* 🦌🎅) with the required typeclasses listed for each of our data types. As a result, we would have:

```
data Gift = Gift
    { giftId :: Int
    , giftType :: GiftType
    } deriving (Eq, Ord)

data GiftType
    = Candies CandyCounter
    | Toy Name
    deriving (Eq, Ord)

newtype Name = Name
    { unName :: Text
    } deriving (Eq, Ord)

newtype CandyCounter = CandyCounter
    { unCandyCounter :: Int
    } deriving (Eq, Ord)
```

And now magically you can compare any values of the declared types.

```
ghci> :{
ghci| compare
ghci|     (Gift 1 (Toy $ Name "Strawberry Bear"))
ghci|     (Gift 2 (Candies $ CandyCounter 1000))
ghci| :}
LT
```

But we are not magicians, and we are not here to show juggles, instead, we'd better show what is actually going on. As we know, the compiler should generate all those instances for us during the compilation. And we can check that by looking into the generated code to make sure that this is what is really happening. To see that, all you need to do is to specify the corresponding GHC option (`ddump-deriv`), which will output the code for you.

When we compile the following code with the helpful GHC options enabled

```
-- Candies.hs file content
{-# OPTIONS_GHC
    -ddump-deriv
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
#-}

module Candies where


newtype CandyCounter = CandyCounter
    { unCandyCounter :: Int
    } deriving (Eq, Ord)
```

And run the `ghc Candies.hs` command, it outputs the derived instances. Specifically:

```
==================== Derived instances ====================
Derived class instances:
  instance Eq CandyCounter where
    (==)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((==) @Int) ::
          CandyCounter -> CandyCounter -> Bool
    (/=)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((/=) @Int) ::
          CandyCounter -> CandyCounter -> Bool

  instance Ord CandyCounter where
    compare
      = coerce
          @(Int -> Int -> Ordering)
          @(CandyCounter -> CandyCounter -> Ordering)
          (compare @Int) ::
          CandyCounter -> CandyCounter -> Ordering
    (<)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((<) @Int) ::
          CandyCounter -> CandyCounter -> Bool
    (<=)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((<=) @Int) ::
          CandyCounter -> CandyCounter -> Bool
    (>)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((>) @Int) ::
          CandyCounter -> CandyCounter -> Bool
    (>=)
      = coerce
          @(Int -> Int -> Bool) @(CandyCounter -> CandyCounter -> Bool) ((>=) @Int) ::
          CandyCounter -> CandyCounter -> Bool
    max
      = coerce
          @(Int -> Int -> Int) @(CandyCounter -> CandyCounter -> CandyCounter) (max @Int) ::
          CandyCounter -> CandyCounter -> CandyCounter
    min
      = coerce
          @(Int -> Int -> Int) @(CandyCounter -> CandyCounter -> CandyCounter) (min @Int) ::
          CandyCounter -> CandyCounter -> CandyCounter
```

Wow, imagine writing all that manually! Let's disassemble this dumped information piece by piece, to make sure that we get something sane when using deriving.

If we were writing the `Eq` instance for `CandyCounter` data type, we would do it like this:

```
instance Eq CandyCounter where
    (==) :: CandyCounter -> CandyCounter -> Bool
    CandyCounter x == CandyCounter y = x == y
```

Basically, we delegate the comparison of two `CandyCounter`s to the equality check of the underlying integers.

In contrast, here the the generated piece responsible for the same instance:

```
instance Eq CandyCounter where
    (==) = coerce
        @(Int -> Int -> Bool)
        @(CandyCounter -> CandyCounter -> Bool)
            ((==) @Int) :: (CandyCounter -> CandyCounter -> Bool
```

Even if this instance looks completely different from what we've implemented manually, it has the same behaviour. GHC generates a more efficient implementation using the fact that `CandyCounter` and `Int` have the same runtime representation. The implementation uses the instance of the `Coercible` typeclass which will be described in more details later. The implementation of the `Ord` typeclass for the `CandyCounter` newtype follows the same principles, except `Ord` has more methods than `Eq`.

Objectively, nothing magical is happening here, as we see GHC is creating quite sensible instances for our data types. But the amount of work that you would need to do in case of manual work, is too damn high.

---

It is nice that we can use the deriving in such an effortless way, but, indeed, the reasonable question that you might have at this point is the following - How does the usage of this feature affect our compiling times?

And, honestly, we don't know a solid answer. It is likely that deriving has its effect, but there are no official measurements on this topic. However, we can give a couple of examples of the degradation of compile-time, which you can try at home (though, not recommended).

If you create a huge enum data type with [~200 constructors][9] (maybe the list of all countries) and then try to derive the `Generic` instance for it, then you can manage to watch 0.5-1 episodes of your favourite TV-series (depending on your choice) while waiting for the build to finish.

Another interesting example you can check is the [brilliant work of Taylor][10] where he gives very interesting and honest results with [the performance comparison of different types][11] using different instance generation mechanisms in Haskell, including the deriving mechanism.

---

The process of the code generation through the deriving mechanisms is the piece of work by itself. We are not going to get into details of the exact description of how it works in GHC (mostly because it is very complicated to get into it for curious strangers like us). But if you are keen to learn more, here is where GHC developers keep all the good stuff and many interesting notes as well:

-   [Deriving implementation in GHC][12].

## Strategies[🔗][13]

In addition to plain deriving clauses, you can use deriving with a specific strategy. Let's see what it means.

**Strategies** are introduced into GHC by the [DerivingStrategies][14] language extension. The GHC documentation gives a concise description of this feature:

Deriving strategies grant users finer-grained control over how instances may be derived.

GHC user guide

Strategy defines the way of the instance generation. When you write `deriving SomeClass`, the compiler needs to figure out the plan of how it is going to create the instance, because in some situations the same typeclass can be derived in multiple ways. And a specific deriving method is called **strategy** .

Strategy is the implicit or explicit way to tell the compiler what kind of the deriving you want for the particular typeclass.

When GHC sees a deriving clause, it associates a strategy for all typeclass listed in the deriving clause. This process is called strategy resolution. If you don't specify the strategy explicitly, GHC uses an algorithm to find out a proper strategy. Otherwise, it matches explicitly specified strategy with the possibility to derive a typeclass using this strategy.

Currently, there are four kinds of deriving strategies:

-   **stock**
-   **newtype**
-   **anyclass**
-   **via**

If you want to specify a strategy manually, you need to add an extra keyword after the `deriving` keyword, e.g. `deriving stock Show`. Precisely, the syntax is as follows:

```
{-# LANGUAGE DerivingStrategies #-}

(data|newtype) Data = Data Declaration
    deriving STRATEGY (TypeClasses, Separated, By Comma)
    deriving STRATEGY (Or, Separate, Clauses)
    deriving (And, Even, Without, A, Strategy)
```

> ☝️ It is only possible to define several deriving clauses only when the `DerivingStrategies` extension is enabled.

Explicit strategy marking is a quite recent feature. Even though GHC knew the concept of "strategies" for quite some time, the users gained the ability to specify the strategy only in GHC 8.2.1 when the `DerivingStrategies` language extension landed in the compiler.

The extension was introduced due to the inability to correctly figure out the strategy by GHC on its own in some cases. Later, we will look into such situations, but for now, it is important to understand that specifying the strategy is important for getting expected and determined behaviour.

Specifically, writing a strategy explicitly has the following benefits:

-   Resolve deriving ambiguity
-   Help compiler to avoid the possibility of the mis-strategy
-   Specify the resulting instance behaviour
-   Take control over the performance of generated instances, as different strategies vary in that aspect

We will look into each of the unique deriving strategies and their meaning while discovering all existing ways of the deriving mechanisms in the coming sections.

> ☝️ From now on, we assume that the `DerivingStrategies` is enabled in all the code we show, unless the otherwise is mentioned.

## Standard deriving[🔗][15]

Let's start our journey to the world of deriving with looking into fundamental deriving features. GHC is capable of deriving instances for some standard typeclasses out-of-the-box and without any additional extensions as per Haskell specification.

The simplest and most common deriving clause looks like this:

```
data Elf = Elf Text Int
    deriving (Show, Eq)
```

By writing the above code, we define the `Elf` data type, and at the same time, we are also asking the compiler to generate the `Show` and `Eq` instances for `Elf`.

The Haskell report mentions standard typeclasses that each Haskell compiler must be able to derive. All other means of deriving are represented by other compiler features - [extensions][16]. Deriving of standard typeclasses is done using the **stock** strategy, though, the report doesn't mention the strategy, since it is not aware of other deriving ways.

According to the Haskell 2010 report, a Haskell compiler should be able to derive the following typeclasses:

-   `Show` - for all data types
-   `Read` - for all data types
-   `Eq` - for all data types
-   `Ord` - for all data types
-   `Enum` - for enumerations (i.e. datatypes having only nullary constructors)
-   `Bounded` - for enumerations and single-constructor data types
-   `Ix` - for enumerations and single-constructor data types, whose constituent types are instances of Ix. A Haskell implementation must provide Ix instances for tuples up to at least size 15.

> Standard typeclasses for deriving are described in the following sections of the Haskell 2010 report:
> 
> -   [Deriving][17]
> -   [`Ix` typeclass deriving][18]

> ☝️ To derive instances for standard typeclasses, **all** fields of constructors must also have corresponding instances. All primitive data types already come with the standard instances.

So, as a Haskell user, you are able to derive the above type classes for any of the data types minding the individual restrictions.

As mentioned, GHC uses the **stock** strategy for deriving standard typeclasses. So the following two data type definitions have the same meaning:

```
data ChristmasTreats = Candies | Chocolate | MincedPie
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

data ChristmasTreats = Candies | Chocolate | MincedPie
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Ix)
```

However, in some cases, you can use a different strategy for deriving standard typeclasses. We will cover other deriving mechanisms in the following sections.

## Auto derived[🔗][19]

The previous section describes the standard typeclasses that could be derived without any extension in a very straightforward way. But there is even a simpler deriving process in GHC that we don't see, and rarely think about.

There are a few very **special** built-in typeclasses for which GHC always creates instances without any actions required from the programmer side. At the moment, the list of such typeclasses includes:

-   `Typeable`
-   `Coercible`
-   `HasField`

Automatic derivation of these typeclasses is done to discourage people from writing these instances manually, as these instances are too critical for the correct work of the program. So GHC generates such instances following its own rules. Despite that, users are still able to write manual instances in some cases, and the rules are provided for each typeclass individually.

### Typeable[🔗][20]

The [Typeable][21] typeclass provides a way to know the type of a value in runtime. GHC has special rules around working with `Typeable`. To give a quick example of `Typeable` usage, let's look at the function that takes a value and returns a representation of its type in runtime:

```
ghci> import Type.Reflection
ghci> :t typeOf
typeOf :: Typeable a => a -> TypeRep a
ghci> typeOf True
Bool
ghci> typeOf [3, 1, 2]
[Integer]
```

From the usage example, you can see how important it is to have such instances out-of-the-box. Moreover, GHC does not allow to establish such instances at all.

Before GHC-7.10 it was indeed necessary to enable [DeriveDataTypeable][22] to have these instances for your data types, but currently, it is an error if you try to manually write the `Typeable` typeclass.

### Coercible[🔗][23]

[Coercible][24] is a magical typeclass that allows converting values of different types that have the same runtime representation. It plays an important role in the **newtype** deriving strategy which we are going to cover later. By the definition:

> Coercible is a two-parameter class that has instances for types `a` and `b` if the compiler can infer that they have the same representation.

Meaning, its purpose for us is to be able to normalise the representation of some two types to the one that is clear to the compiler.

For instance, if we write code like this:

```
newtype CandyCounter = CandyCounter Int
```

GHC automatically generates the following instances for us:

```
instance Coercible CandyCounter Int
instance Coercible Int          CandyCounter
```

And the main benefit that the typeclass gives us is the `coerce` function - fast and safe way to convert values between types `CandyCounter` and `Int`.

Since GHC also provides `Coercible` instances for functions, we can now understand the `Eq` instance for `CandyCounter` from before:

```
instance Eq CandyCounter where
    (==) = coerce (==)
```

In this case, the `coerce` function behaves like the transformer from `Int -> Int -> Bool` to `CandyCounter -> CandyCounter -> Bool`. Since `CandyCounter` is a newtype, it literally has the same representation as `Int` by definition of the newtype, so it is not a big deal for the compiler to apply this coercion and delegate the work to `Int`s. And that is literally what is written in the generated code if we omit all type applications!

> 👩🔬 In some cases, you may want to write Coercible instances manually or control their behaviour using type roles, but that's a tale for another evening.

### HasField[🔗][25]

[HasField][26] is another magical built-in typeclass. Its role is to provide a polymorphic function for record getters.

For example, when you have a record data type like this one:

```
data Elf = Elf
    { name :: Text
    , age  :: Int
    }
```

GHC generates the following instances for you:

```
instance HasField "name" Elf Text where
    getField :: Elf -> Text
    getField = name

instance HasField "age" Elf Int where
    getField :: Elf -> Int
    getField = age
```

And you can use it in the following manner:

```
ghci> getField @"name" Elf{ name = "Tickle Sparklepants", age = 142 }
"Tickle Sparklepants"
ghci> getField @"age" Elf{ name = "Merry Snowball", age = 251 }
251
```

The `HasField` instances help define general instances of some other typeclasses. In the future, they are going to play an important role for the [RecordDotSyntax][27] proposal to make Haskell records more ergonomic.

## Derive Whatever[🔗][28]

In addition to the basic typeclasses like `Show` and `Eq`, GHC can derive more standard typeclasses with the **stock** strategy, but such compiler capabilities require enabling separate extensions for each such typeclass.

This chapter covers standard typeclasses with special extensions (as well as some GHC-specific classes like `Data` and `Generic`), their corresponding extensions and examples of the generated instances. We are not going to explain the meaning and usage of each typeclass separately, but instead, provide auto-generated derived instances to give the idea of each deriving clause.

### Functor[🔗][29]

We have the following data type that derives `Functor`:

```
data Gift a
   = None
   | Some a
   deriving stock (Functor)
```

And GHC produces the following instance:

```
instance Functor Gift where
    fmap f None = None
    fmap f (Some a1) = Some (f a1)
    (<$) z None = None
    (<$) z (Some a1) = Some ((\ b1 -> z) a1)
```

### Foldable[🔗][30]

We have the following data type that derives `Foldable`:

```
data Gift a
   = None
   | Some a
   deriving stock (Foldable)
```

And GHC produces the following instance:

```
instance Foldable Gift where
    foldr f z None = z
    foldr f z (Some a1) = f a1 z
    foldMap f None = mempty
    foldMap f (Some a1) = f a1
    null None = True
    null (Some _) = False
```

### Traversable[🔗][31]

We have the following data type that derives `Traversable`:

```
data Gift a
   = None
   | Some a
   deriving stock (Functor, Foldable, Traversable)
```

> ☝️ To derive `Traversable`, a data type must also have the `Functor` and `Foldable` instances, so we derive them as well.

And GHC produces the following instance:

```
instance Traversable Gift where
    traverse f None = pure None
    traverse f (Some a1) = fmap (\ b1 -> Some b1) (f a1)
```

### Generic and Generic1[🔗][32]

> If you want to have Generic instances, you should **always derive** them instead of defining manually. Not only it is very difficult to write them manually, but also extremely error-prone.

We have the following data type that derives `Generic` and `Generic1`:

```
import GHC.Generics (Generic, Generic1)

data Gift a
   = None
   | Some a
   deriving stock (Generic, Generic1)
```

And GHC produces the following instances:

```
==================== Derived instances ====================
Derived class instances:
  instance forall a. Generic (Gift a) where
    from x
      = M1
          (case x of
             None -> L1 (M1 U1)
             Some g1 -> R1 (M1 (M1 (K1 g1))))
    to (M1 x)
      = case x of
          (L1 (M1 U1)) -> None
          (R1 (M1 (M1 (K1 g1)))) -> Some g1

  instance Generic1 Gift where
    from1 x
      = M1
          (case x of
             None -> L1 (M1 U1)
             Some g1 -> R1 (M1 (M1 (Par1 g1))))
    to1 (M1 x)
      = case x of
          (L1 (M1 U1)) -> None
          (R1 (M1 (M1 g1))) -> Some (unPar1 g1)


Derived type family instances:
  type Rep (Gift a) = D1
                        ('MetaData "Gift" "Deriving" "main" 'False)
                        (C1 ('MetaCons "None" 'PrefixI 'False) U1
                         :+: C1
                               ('MetaCons "Some" 'PrefixI 'False)
                               (S1
                                  ('MetaSel
                                     'Nothing
                                     'NoSourceUnpackedness
                                     'NoSourceStrictness
                                     'DecidedLazy)
                                  (Rec0 a)))
  type Rep1 Gift = D1
                     ('MetaData "Gift" "Deriving" "main" 'False)
                     (C1 ('MetaCons "None" 'PrefixI 'False) U1
                      :+: C1
                            ('MetaCons "Some" 'PrefixI 'False)
                            (S1
                               ('MetaSel
                                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                               Par1))
```

The `Generic` typeclass plays an important role in the **anyclass** deriving strategy which we are going to see later.

### Data[🔗][33]

We have the following data type that derives `Data`:

```

import Data.Data (Data)

data Gift a
   = None
   | Some a
   deriving stock (Data)
```

And GHC produces the following instance:

```
instance forall a. Data a => Data (Gift a) where
    gfoldl k z None = z None
    gfoldl k z (Some a1) = (z Some `k` a1)
    gunfold k z c
      = case constrIndex c of
          I# 1# -> z None
          _ -> k (z Some)
    toConstr None = $cNone
    toConstr (Some _) = $cSome
    dataTypeOf _ = $tGift
    dataCast1 f = gcast1 f

  $tGift :: DataType
  $cNone :: Constr
  $cSome :: Constr
  $tGift = mkDataType "Gift" [$cNone, $cSome]
  $cNone = mkConstr $tGift "None" [] Prefix
  $cSome = mkConstr $tGift "Some" [] Prefix
```

### Lift[🔗][34]

> The `Lift` typeclass is very important for the [TemplateHaskell][35] extension.

We have the following data type that derives `Lift`:

```
import Language.Haskell.TH.Syntax (Lift)

data Gift a
   = None
   | Some a
   deriving stock (Lift)
```

And GHC produces the following instance:

```
instance forall a. Lift a => Lift (Gift a) where
    lift None = conE (mkNameG_d "main" "Deriving" "None")
    lift (Some a1)
      = appE (conE (mkNameG_d "main" "Deriving" "Some")) (lift a1)
```

## Newtypes[🔗][36]

newtype

Any typeclass implemented by the wrapped type

Newtypes are one of the essential Haskell features. They enable creating safe, modular and performant interfaces and are often used in different [programming patterns][37]. And, what is more important in the context of this blog post, deriving is made extremely easy for newtypes due to their nature.

If you have a newtype like this one:

```
newtype GiftCount = GiftCount Int
```

you know that it has the same runtime representation as `Int` by the definition of the newtypes in Haskell. It is basically `Int`, but, from the compiler point of view, you need to add additional wrapping and unwrapping when converting between `GiftCount` and `Int`.

Since `GiftCount` is a completely new data type for GHC, you need to define an instance explicitly. However, the primitive type `Int` already implements many useful instances, and would make sense to reuse them for your types as it is a custom wrapper around `Int`.

In other words, all instances would include lots of wrapping and unwrapping. Fortunately, GHC makes it possible for us: for each newtype you can simply derive **any typeclass** that already has a defined instance for the underlying type.

For example, for the above newtype we can derive the `Num` instance, even though it is not a standard class, as `Int` already has such an instance. To specify strategy explicitly, use the **newtype** keyword after the **deriving** keyword:

```
newtype GiftCount = GiftCount Int
    deriving newtype (Num)
```

If you just try to compile the above code, you will see the following error message:

```
<interactive>:14:44: error:
    • Can't make a derived instance of 'Num GiftCount':
        'Num' is not a stock derivable class (Eq, Show, etc.)
        Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
    • In the newtype declaration for 'GiftCount'
```

That's happening because the compiler is not capable of deriving non-standard typeclasses for newtypes. You need to enable the `GeneralizedNewtypeDeriving` extension to derive any typeclass for newtypes. After you enable it, you can see that code starts working as expected:

```
ghci> GiftCount 1 + GiftCount 2
GiftCount 3
ghci> GiftCount 5 - GiftCount 3
GiftCount 2
```

When you don't specify a strategy explicitly for newtypes, GHC can use the **stock** strategy for standard typeclasses, and the **newtype** strategy for other typeclasses (unless you have the `DeriveAnyClass` extension enabled, which will be covered in the next chapter).

Now you can start understanding the importance of specifying the strategy explicitly. The following two data types that derive the `Show` instance using different strategies have different behaviour:

```
newtype Wrap1 = Wrap1 Int
    deriving (Show)  -- defaults to stock here

newtype Wrap2 = Wrap2 Int
    deriving newtype (Show)
```

You can observe the difference in their behaviour using GHCi:

```
ghci> Wrap1 42
Wrap1 42
ghci> Wrap2 42
42
```

For the typeclasses like `Show`, a strategy can change the behaviour. And for the typeclasses like `Eq`, a strategy can affect performance.

If you derive `Eq` for `newtype` using the `stock` strategy:

```
newtype Gift = Gift String
    deriving stock (Eq)
```

the derived instance looks like this:

```
instance Eq Gift where (==) (Gift a1) (Gift b1) = ((a1 == b1))
```

But if you use the `newtype` strategy explicitly:

```
newtype Gift = Gift String
    deriving newtype (Eq)
```

The instance, generated by GHC has the following shape:

```
  instance Eq Gift where
    (==)
      = coerce
          @(String -> String -> Bool)
          @(Gift -> Gift -> Bool)
          ((==) @String) ::
          Gift -> Gift -> Bool
    (/=)
      = coerce
          @(String -> String -> Bool)
          @(Gift -> Gift -> Bool)
          ((/=) @String) ::
          Gift -> Gift -> Bool
```

The coerce function implements instances with zero-runtime overhead, using the fact that a newtype and the wrapped type has the same runtime representation. See the [Coercible][38] section for more details.

---

The newtype strategy allows deriving even more typeclasses for newtypes which saves us from writing tons of boilerplate (and doing a lot of (un)wrapping as on the Boxing Day), does not affect instance performance, and yet allows us to use all the benefits of the newtypes in Haskell.

At the same time, it is important to know where to use **stock** vs **newtype** strategy to control expected behaviour and performance.

## Any class derivations[🔗][39]

anyclass

Any typeclass with default signatures

The **anyclass** deriving strategy is another crucial tool in the deriving toolbox of the Haskell developer. As the name suggests, this feature allows deriving **any** typeclass.

When deriving a typeclass using the **anyclass** strategy, GHC simply generates an instance declaration with no explicitly-defined methods, e.g. `instance Foo Bar`. Instead, the default definitions of methods are used. You can write the default definitions that will be used either always or only under some conditions, specified by additional constraints for the method, when introducing a typeclass.

For specifying default signatures, that will be used only when a data type satisfy the requirements for the default definition, you need to:

-   Provide default definitions of the required typeclass methods using the [DefaultSignatures][40] extension in the corresponding typeclasses.
-   Enable the `DeriveAnyClass` extension.
-   Derive your typeclass using the **anyclass** strategy.

A hypothetical transparent example of a typeclass with the default signatures may be an alternative definition of the `Eq` typeclass, where the default equality definition compares `Strings` - the textual representation of the values based on the `Show` typeclass. Let's define such new `Eq` typeclass:

```
class MyEq a where
    equal :: a -> a -> Bool

    default equal :: Show a => a -> a -> Bool
    equal x y = show x == show y
```

And then you can derive instances of the `MyEq` typeclass if you have the `Show` instance for your data type. The syntax is the following:

```
data Gift = Gift Text Int
    deriving stock (Show)
    deriving anyclass (MyEq)
```

If you don't enable the `DeriveAnyClass` extension, you get the following error:

```
    • Can't make a derived instance of
        'MyEq Gift' with the anyclass strategy:
        Try enabling DeriveAnyClass
    • In the data declaration for 'Gift'
   |
27 |     deriving anyclass (MyEq)
   |                        ^^^^
```

After enabling the extension, as the compiler suggests, the generated code looks like this:

```
==================== Derived instances ====================
Derived class instances:
  instance MyEq Gift where

==================== Filling in method body ====================
MyEq [Gift]
  equal = $dmequal @(Gift)
```

Since this looks a bit different from what we saw before, let's try to understand it piece by piece.

The code with the anyclass deriving strategy is equivalent to the following code on older GHC versions without the `DeriveAnyClass` extension:

```
data Gift = Gift Text Int
    deriving (Show)

instance MyEq Gift
```

And this is what the compiler does when you use the **anyclass** strategy.

The part with *Filling in method body* is written in the core (intermediate GHC syntax after desugaring) and uses the internal details of typeclasses implementation. But that's a completely different topic. For now, we just need to be sure that the default definitions are reused.

If you don't derive the `Show` instance, you get the following error:

```
    • No instance for (Show Gift)
        arising from the 'deriving' clause of a data type declaration
      Possible fix:
        use a standalone 'deriving instance' declaration,
          so you can specify the instance context yourself
    • When deriving the instance for (MyEq Gift)
   |
51 |     deriving anyclass (MyEq)
   |                        ^^^^
```

### Generic anyclass[🔗][41]

In Haskell, a lot of default definitions are based on the [Generic][42] typeclass. Generics represent structural polymorphism - uniform description of the shape of all possible Haskell data types. Hereby, if you write a default definition that works for any data type with the `Generic` instance, you can easily derive your typeclass for *any* data type (but be mindful of the compilation cost).

One common example is the pair of `ToJSON/FromJSON` typeclasses from the `aeson` library that allow converting any data types to JSON representation and back automatically. Typical usage is illustrated by the following code snippet:

```
data Elf = Elf
    { name        :: Text
    , workGroupId :: Int
    } deriving stock (Generic)
      deriving anyclass (ToJSON, FromJSON)
```

### Exception anyclass[🔗][43]

Another common typeclass that uses the **anyclass** deriving strategy is `Exception`. The `Exception` typeclass doesn't use the `DefaultSignatures` extension, all its methods have a default implementation. However, a data type implementing `Exception` must also have `Show` and `Typeable` instances. Since `Typeable` is provided automatically by GHC, we only need to derive `Show` in addition to deriving `Exception`.

You can easily derive the `Exception` typeclass like this:

```
data ChristmasException = ChristmasException String
   deriving stock (Show)
   deriving anyclass (Exception)
```

After you derive, you can throw your data types as any other exception:

```
ghci> throwIO (ChristmasException "Danger! Run out of candy canes")
*** Exception: ChristmasException "Danger! Run out of candy canes"
```

### Anyclass ambiguity[🔗][44]

One of the most important applications of deriving strategies is the ability to disambiguate between the **newtype** strategy and **anyclass** strategy. This was one of the main motivations for implementing the `DerivingStrategies` extension in the first place.

Let's look at an example of such an ambiguous situation:

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


newtype LetterToSanta = LetterToSanta Text
    deriving (Show, Generic, Eq, IsString, ToJSON, FromJSON)
```

We've created the `LetterToSanta` newtype and we want to derive a few typeclasses for it, including `ToJSON` and `FromJSON`. Santa usually receives a lot of requests around Christmas, so to help him to handle all requests, elves wrote a service that can process such requests in the JSON format.

But the problem here is that GHC doesn't know how to derive `ToJSON` and `FromJSON` typeclasses. The compiler has two choices:

-   Use the **newtype** strategy
-   Use the **anyclass** strategy

They are both equally valid. Currently, GHC chooses a default for you and produces a warning:

```
    • Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled
      Defaulting to the DeriveAnyClass strategy for instantiating ToJSON
      Use DerivingStrategies to pick a different strategy
    • In the newtype declaration for 'LetterToSanta'
   |
18 |     deriving (Show, Generic, Eq, IsString, ToJSON, FromJSON)
   |                                            ^^^^^^
```

You can suppress this warning, but it can be dangerous. For example, the choice of default strategy in the `IsString` typeclass leads to the runtime error:

```
ghci> print ("Hello, Santa!" :: LetterToSanta)
LetterToSanta "*** Exception: Main.hs:22:34-41: No instance nor default method for class operation fromString
```

So, as the warning text recommends, you should enable the `DerivingStrategies` extension and specify strategies explicitly. For the `LetterToSanta` data type, they can look like this:

```
newtype LetterToSanta = LetterToSanta Text
    deriving stock (Show, Generic)
    deriving newtype (Eq, IsString)
    deriving anyclass (ToJSON, FromJSON)
```

## Via[🔗][45]

via

Any typeclass implemented by the "via" data type

The **via** deriving mechanism is one of the newest accepted features in GHC. Even though it is rather new, many developers widely use it in both libraries and applications already.

Introduced in GHC version 8.6, this feature can be enabled with the `DerivingVia` extension.

The main idea around this way of deriving is that the generated instance may resemble very closely an instance of some other data type similar to yours, which you can specify as the reference point of the instance you would like to see.

You can see how the user guide describes the **via** technique:

This allows deriving a class instance for a type by specifying another type of equal runtime representation (such that there exists a Coercible instance between the two: see The Coercible constraint) that is already an instance of the that class.

GHC user guide

`DerivingVia` introduces the new strategy of deriving with the keyword **via**. Important to mention that due to its unique way of work, it requires to specify additional information during deriving - the data type name through which you want to derive instances for your data type.

Let's get the idea through the example first. Imagine that we have a class that describes beards:

```
class Beard faceOfTheWinterHoliday where
    beardDescription :: faceOfTheWinterHoliday -> String
```

And we already know what kind of beard does Santa have:

```
newtype Santa = Santa String
    deriving newtype (Show)

instance Beard Santa where
    beardDescription (Santa name) = name <> " has long white beard"
```

When the times come for Father Frost to take the action, we understand, that they have pretty much the same representation holiday-wise, and their beard could also be unified:

```
newtype FatherFrost = FatherFrost String
    deriving newtype (Show)
    deriving Beard via Santa
```

And the created instance for `FatherFrost` is similar to the `Santa`s one:

```
  instance Beard FatherFrost where
    beardDescription
      = coerce
          @(Santa -> String)
          @(FatherFrost -> String)
          (beardDescription @Santa) :: FatherFrost -> String
```

This looks quite handy for the instance deriving, which gives you a lot of control over what you want to achieve with the minimal amount of work and boilerplate. However, it requires you to understand when and how to do that.

We have an easy answer on this one. This requires you just 3 steps:

1.  Create a corresponding newtype.
2.  Write the instance for the desired typeclass manually.
3.  Write the deriving via this newtype for your data type.

This feature also is very important for the libraries that expose typeclasses as part of the library interface. `DerivingVia` makes it possible to provide additional data type with the instance of your typeclass, so that users could use it for their data type instances deriving.

For real-life example usage of DerivingVia, you can refer to the [deriving-aeson][46] Haskell library that allows customising `ToJSON` and `FromJSON` instances by providing helpful newtypes.

> A careful reader may notice that both `DefaultSignatures` and `DeriveAnyClass` can be considered as a special case of `DerivingVia`. Indeed, instead of providing only one default definition of a method, you can define multiple newtypes with their own definitions, and later use the **via** strategy to specify the behaviour explicitly.

## Standalone deriving[🔗][47]

If there is a type error, it is your problem.

GHC user guide, `StandaloneDeriving`

Let's now see the other side of deriving instance declarations. In Haskell, it is possible to define instances anywhere in the code, not exactly near the data type definition. If the instance is in the other module/package/project than typeclass and data type then such instances are called [*orphan instances*][48]. Such instances are usually discouraged, and there is even the GHC warning to remind users about that. But sometimes it is unavoidable. And that means that we need a way to do the same for `deriving` as well.

Luckily, GHC provides such a feature through the [StandaloneDeriving][49] extension for quite a long time already (since version 6.8.1). But this is not the only use case for the standalone deriving. You can use it for:

-   Orphan instance deriving
-   GADT instance deriving
-   Different deriving instances for specified parameters in the parameterised data types (e.g. for the data type `Foo x` you want to have separate `Foo (Maybe a)` and `Foo (NonEmpty a)` instances, which is not possible during the standard derivation during the data type declaration).

The syntax is very alike the ordinary instance declaration with a few differences. Let's look at it:

```
deriving [STRATEGY] instance (Constraints) => TypeClass DataType
```

The `STRATEGY` is optional, but we recommend specifying it anyway. A few examples of standalone deriving with explicit strategies:

```
data Gift a
    = None
    | Some a

deriving stock instance (Show a) => Show (Gift a)
deriving via (Last (Gift a)) instance Semigroup (Gift a)
```

One thing to keep in mind is that for any deriving strategy but `DerivingAnyClass` you need to have all the constructors of the data type in scope as the generated instances would use them.

## Empty Deriving[🔗][50]

As we are talking all about deriving today, it is fair to mention another feature provided by GHC - ability to derive instances of empty data structures.

Haskell allows you to declare the data types without any constructors, and even more, the reports suggest the compiler to enable users to do this since the report Haskell2010 (for Haskell98 you need to enable `EmptyDataDecl` manually).

Here is the example of such data type:

The usefulness of such constructions will not be discussed here, though you can think of a few use cases for them.

The thing is that the Haskell report does not allow to derive instances of standard typeclasses for such structures. Here is what it says about it:

If the data declaration has no constructors, then no classes are derivable.

However, sometimes it is handy to be able to derive instances for empty data types. And GHC lets you do it when you enable [EmptyDataDeriving][51] extension:

```
{-# LANGUAGE EmptyDataDeriving #-}

data EmptyGiftForNaugthy
    deriving stock (Show, Eq)
```

And this will generate the following instance code:

```
instance Show EmptyGiftForNaugthy where
    showsPrec _ z_a84K = case z_a84K of

instance Eq EmptyGiftForNaugthy where
    (==) _ z_a84L = True
```

Note, that this extension is only required for the standard typeclasses (described in the Standard deriving section). For all other ways of deriving (including `StandaloneDeriving`), you do not need to enable that. All you need is to include the corresponding necessary extension as for any other data types.

## Best practices with Deriving[🔗][52]

This section contains recommendations on using **deriving** when writing Haskell general-purpose libraries and production applications. We gathered this information during our work at different projects and found the optimal workflows that help developers to be more productive. We also were closely watching the practices that are used by other Haskell engineers to get the best of deriving.

**Always specify the deriving strategy.** Since GHC 8.8, you can enable the [-Wmissing-deriving-strategies][53] to warn on implicit deriving strategies.

**Use `-Wderiving-defaults` for your code.** Since GHC 8.10, a new warning, [-Wderiving-defaults][54] was introduced, to draw attention to ambiguous deriving clauses. This warning is enabled by default if you use `-Wall`. And it would alarm you each time when the compiler resolves the unspecified strategy on its own.

**Derive `Show` and `Eq` for all data types whenever possible.** Deriving these typeclasses doesn't require any effort from your side, as well as it does not affect the compile-time insignificantly. However, these instances allow inspecting and testing values of the data types and could be beneficial to anyone who would use these data types in any way.

**The `Show` typeclass must be derived instead of being written manually.** By convention, the `Show` should represent a Haskell view of a data type. If you need some custom printing for a different purpose (pretty-printing, logging, etc.), you'd better use a custom function or a different typeclass. Having the `Show` instance derived also allows you to utilize packages like [pretty-simple][55] to debug Haskell data types easier.

**`Show` and `Read` instances should be aligned.** As an addition to the previous point, `Show` and `Read` instances for a data type must satisfy the roundtrip property: `read . show ≡ id`. If you derive the `Show` instance, and if you need `Read`, it also makes sense to derive `Read` as well instead of writing it manually. The `Read` typeclass is not supposed to be used for efficient parsing with helpful error messages, though it is still used a lot due to convenience and availability out-of-the-box.

**Use the *newtype* strategy whenever possible and when it makes sense.** For performance reasons, derive typeclasses like `Eq`, `Ord`, `Enum`, `Bounded`, `Hashable` using the **newtype** strategy. When it works for your use case, use the **newtype** strategy when deriving typeclasses from external libraries, e.g. `ToField/FromField` from [postgresql-simple][56], `ToJSON/FromJSON` from [aeson][57].

**Derive `Generic` instances if you see possible applications of it for your data types.** For example, if you have a lot of record data types, you may want to derive `Generic` to play nicely with the [generic-lens][58] library. However, notice that deriving `Generic` and other typeclasses based on generics can take a significant amount of compile time. Be mindful of the time of developers who are going to use your library or application and be aware of the trade-offs of different solutions. Deriving more typeclasses and using more features can increase compilation times, but can also increase developer productivity. To summarise, in some cases it might be better to write instances of different typeclasses manually, instead of using the **anyclass** strategy based on `Generic`.

---

Based on the above recommendations, you can put the following GHC options into a common stanza of the `.cabal` file for your Haskell package, and get the maximal effect of deriving best-practices:

```
common common-options:

  if impl(ghc >= 8.8)
    ghc-options:       -Wmissing-deriving-strategies
                       -Werror=missing-deriving-strategies

  default-extensions:  DeriveAnyClass
                       DeriveDataTypeable
                       DeriveFoldable
                       DeriveFunctor
                       DeriveGeneric
                       DeriveLift
                       DeriveTraversable
                       DerivingStrategies
                       DerivingVia
                       GeneralizedNewtypeDeriving
```

All the mentioned `default-extensions` are innocent, and won't break anything, even with some other extensions enabled. However, without the `-Werror=missing-deriving-strategies` it is not recommended to enable `DeriveAnyClass` and `GeneralizedNewtypeDeriving` simultaneously due to ambiguity described earlier in the post.

Deriving significantly simplifies the life of a Haskell developer by reducing the need to write boilerplate code. But so much code is being derived nowadays that this method of removing boilerplate became boilerplate itself (though in a much more comprehensible way).

In a typical Haskell application, you may end up with a code similar to the below one:

```
-- | Wrapper over the text of an email.
newtype Email = Email { unEmail :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, ToMustache)
    deriving anyclass (FromJSON, ToJSON)

-- | Subject of the email.
newtype Subject = Subject { unSubject :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, ToMustache)
    deriving anyclass (ToJSON, FromJSON)

-- | Wrapper for primary keys in a database table.
newtype Id a = Id { unId :: Int }
    deriving stock (Show, Generic)
    deriving newtype ( Eq, Ord, Hashable, FromField, ToField, ToMustache
                     , FromHttpApiData, ToHttpApiData)
    deriving anyclass (FromJSON, ToJSON)
```

Once upon a time there was a GHC proposal, addressing this issue, but it was closed:

-   [DerivingAlias proposal][59]

So there is still an open question about how to improve on this side. Maybe you can come up with a solution that would help to resolve this question as well! We encourage you to propose your thoughts through the GHC proposals process.

## Summary[🔗][60]

As we showed, deriving is a powerful and helpful technique that you can safely use in your code. It gives developers a lot of opportunities in the way to handle instances for the data types.

Yet, this is an enormous chapter of the GHC, with a lot of interesting aspects. And the knowledge of all these stuff could help you to be a better Haskell developer.

To consolidate our today's knowledge of deriving mechanisms, here is a useful table, that covers all the described techniques in the post:

| Typeclass | Strategy | Extension |
| --- | --- | --- |
| `Show`, `Read`, `Eq`, `Ord`, `Enum`, `Bounded`, `Ix` | stock | none |
| `Typeable`, `Coercible`, `HasField` | none | none |
| `Functor` | stock | `DeriveFunctor` |
| `Foldable` | stock | `DeriveFoldable` |
| `Traversable` | stock | `DeriveTraversable` |
| `Generic`, `Generic1` | stock | `DeriveGeneric` |
| `Data` | stock | `DeriveDataTypeable` |
| Any typeclass implemented by the wrapped type | newtype | `GeneralizedNewtypeDeriving` |
| Any typeclass with default signature | anyclass | `DeriveAnyClass` |
| Any typeclass implemented by the "via" data type | via | `DerivingVia` |

## Quiz: Lock, Stock and Two Smoking Barrels[🔗][61]

This section contains some tasks to practice understanding of deriving strategies as well as some challenges for puzzle-lovers.

### Training 1: Specify strategy[🔗][62]

Specify strategies explicitly in the following code:

```
data GiftType = Toy | Bicycle | Dress | Shoe
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

newtype Email = Email
    { unEmail :: Text
    } deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

newtype App a = App
    { unApp :: ReaderT MyEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader MyEnv)
```

```
data GiftType = Toy | Bicycle | Dress | Shoe
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Ix)

newtype Email = Email
    { unEmail :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable)
      deriving anyclass (FromJSON, ToJSON)
      --       ^^^^^^^^
      --  this can be newtype depending on the use-case

newtype App a = App
    { unApp :: ReaderT MyEnv IO a
    } deriving newtype ( Functor, Applicative, Monad
                       , MonadIO, MonadReader MyEnv
                       )
```

### Training 2: Disambiguate[🔗][63]

Having the following extensions enabled in the module:

```
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

what would happen when compiling with `-Wall` the following code?

```
newtype ToyName = ToyName
    { unToyName :: Text
    } deriving (Show, Read, Generic, IsString, FromJSON, ToJSON)
```

Possible answers:

-   It won't compile
-   There will be (a) warning(s) but it will compile and work properly
-   Warnings and wrong runtime behaviour
-   No warnings during compilation, works as expected
-   No warnings during compilation, runtime error

Warning for defaulting strategy for deriving `IsString`, `FromJSON` and `ToJSON`. Invalid runtime behaviour for the `IsString` instance - throws an exception.

### Puzzle 1: Semigroup and Monoid[🔗][64]

Semigroup became the superclass of `Monoid` in `base-4.11.0.0` (GHC 8.4.1). That means that now it is required for the data type to be a `Semigroup` in order to have an instance of `Monoid`.

Besides, `base` provides the [WrappedMonoid][65] newtype that allows deriving `Semigroup` instance, if you already have a `Monoid` instance. But is it possible to use such a newtype in the wrong way?

What would happen when compiling the following code with `-Wall` and running it?

```
newtype Gift = Gift
    { unGift :: String
    } deriving stock (Show)
      deriving Semigroup via WrappedMonoid Gift

instance Monoid Gift where
    mempty = Gift ""
```

Possible answers:

-   Compiler error
-   Compiler warning
-   Incorrect instance work after termination (exception, wrong results, etc.)
-   Hangs in runtime

The code perfectly compiles and doesn't produce any warnings. Even when enabling the `-Wnoncanonical-monoid-instances` warning, not included in `-Wall`. However, when trying to append two values, we can see that our code hangs (don't try at home!):

```
ghci> Gift "bear" <> Gift "bicycle"
Gift {unGift = "
```

This happens because our hand-written `Monoid` instance doesn't specify the implementation of `mappend`, and by default, it's implemented as an alias to the `(<>)` operator from `Semigroup`. At the same time, by implementing `Semigroup` via `WrappedMonoid`, we explicitly tell the compiler to implement `(<>)` as an alias to `mappend`. That's why we have infinite non-terminating recursion.

### Puzzle 2: Infinite deriving[🔗][66]

Without writing the `Show` instance explicitly, but using only deriving capabilities, implement a `Show` instance for the data type that prints nested string `"Ho"` infinitely. In other words, if you have a data type like this:

```
newtype Infinite = Infinite Int
```

the behaviour of the `show` function for this type should be the following:

```
ghci> Infinite 0
Ho (Ho (Ho (Ho (...
```

```
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

newtype Ho a = Ho a
    deriving stock (Show)

newtype Infinite = Infinite Int
    deriving Show via Ho Infinite
```

## Conclusion[🔗][67]

We hope we explained the deriving mechanism, different strategies and syntax extensions in an accessible way. Now you should be able to reduce even more boilerplate from your code, and with the help of strategies, you would gain a fine-grained control of generated instances.

Let's derive strategically! And don't forget to write a letter to Santa this year ;)

## Sources[🔗][68]

**Haskell2010 Reports**

-   [Chapter 11: Specification of Derived Instances][69]
-   [Chapter 19: Data.Ix][70]

**GHC resources**

-   [GHC Wiki: Deriving Strategies][71]
-   [GHC User Guide: Extensions to the "deriving" mechanism][72]
-   [GHC Deriving Implementation][73]
-   [GHC User Guide: Default deriving strategy][74]
-   [`-Wderiving-defaults` warning][75]
-   [`-Wmissing-deriving-strategies` warning][76]

**Generics**

-   [Haskell Wiki: Generics][77]
-   [GHC Wiki: generic deriving][78]
-   [Hackage: `generic-data`][79]
-   [Hackage: `generic-deriving`][80]

**Typeable**

-   [GHC User Guide: Deriving `Typeable` instances][81]
-   [Typeable - A long journey to type-safe dynamic type representation][82]

**Syntax**

-   [GHC User Guide: StandaloneDeriving][83]
-   [GHC User Guide: EmptyDataDeriving][84]
-   [Haskell Wiki: GHC/Stand-alone deriving declarations][85]

**Orphans**

-   [GHC User Guide: Orphan modules and instance declarations][86]
-   [Haskell Wiki: Orphan instance][87]

[1]: https://kowainik.github.io/posts/deriving#intro
[2]: https://kowainik.github.io/posts/deriving#best-practices-with-deriving
[3]: https://kowainik.github.io/posts/deriving#typeclasses-and-instances
[4]: https://kowainik.github.io/posts/deriving#santa-letters
[5]: https://kowainik.github.io/posts/deriving#haskell-reports
[6]: https://kowainik.github.io/posts/deriving#countable-sets
[7]: https://kowainik.github.io/posts/deriving#motivation
[8]: https://kowainik.github.io/posts/deriving#deriving
[9]: https://gitlab.haskell.org/ghc/ghc/-/issues/5642
[10]: https://taylor.fausak.me/2017/08/09/deriving-type-classes-in-haskell-is-slow/
[11]: https://dev.to/tfausak/how-to-define-json-instances-quickly-5ei7
[12]: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Deriv.hs
[13]: https://kowainik.github.io/posts/deriving#strategies
[14]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies
[15]: https://kowainik.github.io/posts/deriving#standard-deriving
[16]: https://kowainik.github.io/posts/extensions
[17]: https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18200011
[18]: https://www.haskell.org/onlinereport/haskell2010/haskellch19.html
[19]: https://kowainik.github.io/posts/deriving#auto-derived
[20]: https://kowainik.github.io/posts/deriving#typeable
[21]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Type-Reflection.html#t:Typeable
[22]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-data-instances
[23]: https://kowainik.github.io/posts/deriving#coercible
[24]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Coerce.html#t:Coercible
[25]: https://kowainik.github.io/posts/deriving#hasfield
[26]: https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Records.html#t:HasField
[27]: https://github.com/ghc-proposals/ghc-proposals/pull/282
[28]: https://kowainik.github.io/posts/deriving#derive-whatever
[29]: https://kowainik.github.io/posts/deriving#functor
[30]: https://kowainik.github.io/posts/deriving#foldable
[31]: https://kowainik.github.io/posts/deriving#traversable
[32]: https://kowainik.github.io/posts/deriving#generic-and-generic1
[33]: https://kowainik.github.io/posts/deriving#data
[34]: https://kowainik.github.io/posts/deriving#lift
[35]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TemplateHaskell
[36]: https://kowainik.github.io/posts/deriving#newtypes
[37]: https://kowainik.github.io/posts/haskell-mini-patterns#newtype
[38]: https://kowainik.github.io/posts/deriving#coercible
[39]: https://kowainik.github.io/posts/deriving#any-class-derivations
[40]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DefaultSignatures
[41]: https://kowainik.github.io/posts/deriving#generic-anyclass
[42]: https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html#t:Generic
[43]: https://kowainik.github.io/posts/deriving#exception-anyclass
[44]: https://kowainik.github.io/posts/deriving#anyclass-ambiguity
[45]: https://kowainik.github.io/posts/deriving#via
[46]: https://hackage.haskell.org/package/deriving-aeson
[47]: https://kowainik.github.io/posts/deriving#standalone-deriving
[48]: https://wiki.haskell.org/Orphan_instance
[49]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations
[50]: https://kowainik.github.io/posts/deriving#empty-deriving
[51]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-EmptyDataDeriving
[52]: https://kowainik.github.io/posts/deriving#best-practices-with-deriving
[53]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
[54]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/using-warnings.html#ghc-flag--Wderiving-defaults
[55]: https://hackage.haskell.org/package/pretty-simple
[56]: https://hackage.haskell.org/package/postgresql-simple
[57]: https://hackage.haskell.org/package/aeson
[58]: https://hackage.haskell.org/package/generic-lens
[59]: https://github.com/ghc-proposals/ghc-proposals/pull/215
[60]: https://kowainik.github.io/posts/deriving#summary
[61]: https://kowainik.github.io/posts/deriving#quiz-lock-stock-and-two-smoking-barrels
[62]: https://kowainik.github.io/posts/deriving#training-1-specify-strategy
[63]: https://kowainik.github.io/posts/deriving#training-2-disambiguate
[64]: https://kowainik.github.io/posts/deriving#puzzle-1-semigroup-and-monoid
[65]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Semigroup.html#t:WrappedMonoid
[66]: https://kowainik.github.io/posts/deriving#puzzle-2-infinite-deriving
[67]: https://kowainik.github.io/posts/deriving#conclusion
[68]: https://kowainik.github.io/posts/deriving#sources
[69]: https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18200011
[70]: https://www.haskell.org/onlinereport/haskell2010/haskellch19.html
[71]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies
[72]: https://downloads.haskell.org/ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#extensions-to-the-deriving-mechanism
[73]: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Deriv.hs
[74]: https://downloads.haskell.org/ghc/8.10.2/docs/html/users_guide/glasgow_exts.html#default-deriving-strategy
[75]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/using-warnings.html#ghc-flag--Wderiving-defaults
[76]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
[77]: http://www.haskell.org/haskellwiki/Generics
[78]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generic-deriving
[79]: http://hackage.haskell.org/package/generic-data
[80]: http://hackage.haskell.org/package/generic-deriving
[81]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#deriving-typeable-instances
[82]: https://medium.com/@hgiasac/typeable-a-long-journey-to-type-safe-dynamic-type-representation-9070eac2cf8b
[83]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations
[84]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-EmptyDataDeriving
[85]: http://www.haskell.org/haskellwiki/GHC/Stand-alone_deriving_declarations
[86]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#orphan-modules
[87]: https://wiki.haskell.org/Orphan_instance
