# F-algebras

* Part 24 of `Categories for Programmers`
https://bartoszmilewski.com/2017/02/28/f-algebras/

* Category Theory II 8.1: F-Algebras, Lambek's lemma
https://www.youtube.com/watch?v=zkDVCQiveEo&list=PLbgaMIhjbmElia1eCEZNvsVscFef9m0dm&index=15

## Monoids

A **monoid** consists of a carrier set endowed with a single associative binary operation closed over the set.

Monoid
- a carrier set `S`
- 1 associative binary operation (•) closed over the set
- closure: `(∀ a b ∈ S). a • b ∈ S`
- associativity: `(∀ a b c ∈ S). a • (b • c) = (a • b) • c = a • b • c`

We've seen a monoid as a set, as a single-object category, as an object in a monoidal category, and now let's focus on the definition of a monoid as a set `m` with a pair of functions `μ` and `η`.

```hs
μ : m × m -> m
η : 1 -> m
```


```hs
η :: () -> m
μ :: m -> m -> m


    η                   μ
   ----3--->
() ----1---> m -------> m -------> m
   ----2--->

ι :: m
ι = η ()

μ :: m -> m -> m
μ ι y = y
μ x ι = x
μ 1 y = y
μ x 1 = x
μ x y = μ x y


η :: () -> Int
μ ::       Int -> Int -> Int

foo = μ ∘ η
foo :: m -> (() -> m) -> (m -> m -> m) -> m
-- ≅   m ->        m  -> (m -> m -> m) -> m
foo y η μ = let ι :: m = η ()
            in  μ ι y -- y

-- ≅ μ ι   = id
-- ≅ μ ι y = y
```


Since `1` is the terminal object, perhaps it would be better to denote it as the Haskell friendly unit type/value, `()`. Then `η :: () -> m` is the type. Again, `η` is used to select the identity element from the set `m` because we cannot reference it directly (if we could, we would bind it to a constant `η` of the type `m`, i.e. `η :: m`).

```hs
-- we cannot reference the identity element directly, as η :: m
-- so we use the terminal object and its morphisms to m as the "element picker"
η :: () -> m

-- binary assoc mult op closed over m
μ :: (m, m) -> m
-- which, in Haskell, is isomorphic to
μ :: m -> m -> m
```


The `η` function selects the unit (or identity or neutral) element from the carrier set `m`. And the `μ` is an operation that combines a pair of elements, `m × m`, from the set `m`, and returns their product - also an element of `m`. This operations is sometimes called multiplication (although it may not even resamble the proper multiplication operation on numbers; however, it often does).

```hs
μ :: m × m -> m  ≅  μ ∈ m^(m×m)
η :: 1 -> m      ≅  η ∈ m¹

m^(m × m) × m¹ = m^(m × m + 1) = m^(m² + 1)
```

However, not every choice of two functions with these signatures results in a monoid. For that we need to impose additional conditions: *associativity* and *unit laws* - but let's forget about that for a moment and just consider "potential monoids" - a pair of functions is an element in a cartesian product of two sets of functions. We know that these sets may be represented as exponential objects so

```hs
μ ∈ m^(m×m)
η ∈ m¹
```

The cartesian product of these two sets is `m^(m × m) × m¹` and using algebra (which works in every cartesian closed category), rewrite it as `m^(m × m + 1)`.

`m^(m × m) × m¹ = m^(m × m + 1)`

The plus sign stands for the coproduct in 𝗦𝗲𝘁. We have just replaced a pair of functions with a single function - an element of the set `m × m + 1 -> m`. Any element of this set of functions is a potential monoid.

```hs
type Algebra f a = f a -> a
```

The beauty of this formulation is that it leads to interesting generalizations. For instance, how would we describe a group using this language?

A *group* is a monoid with one additional function that assigns the inverse to every element and has the type `m -> m`. As an example, integers form a group with addition as a binary operation, zero as the unit, and negation as the inverse. To define a group we would start with a triple of functions:

```
m × m -> m
    m -> m
    1 -> m
```

As before, we can combine them all into a single set of functions:

`m × m + m + 1 -> m`

We started with one binary operator (addition), one unary operator (negation), and one nullary operator (identity, here zero). We combined them into one function. All functions with this signature define potential groups.

We can go on like this. For instance, to define a ring, we would add one more binary operator and one nullary operator, and so on. Each time we end up with a function type whose left-hand side is a sum of powers (possibly including the zeroth power - the terminal object), and the right-hand side being the set itself.

Now we can go crazy with generalizations. First of all, we can replace sets with objects and functions with morphisms. We can define n-ary operators as morphisms from n-ary products. It means that we need a category that supports finite products. For nullary operators we require the existence of the terminal object. So we need a cartesian category. In order to combine these operators we need exponentials, so that's a cartesian closed category. Finally, we need coproducts to complete our algebraic shenanigans.

Alternatively, we can just forget about the way we derived our formulas and concentrate on the final product. The sum of products on the left hand side of our morphism defines an endofunctor. What if we pick an arbitrary endofunctor `F` instead? In that case we don't have to impose any constraints on our category. What we obtain is called an *F-algebra*.

---


F (F a) -------------> F a
|            F m         |
|                        |
| j                    i |
|                        |
|                        |
↓             m          ↓
F a -------------------> a

For an endofuctor F, considering two algebras (a, i) and (F a, j), is it always the case that the evaluation of (a, i) is also an homomorphism from (F a, j) to (a, i)? m and i are the same?

---

An F-algebra is a triple consisting of an endofunctor `F`, an object `a`, and a morphism `F a -> a`.

The object is often called the carrier, an underlying object or, in the context of programming, the _carrier type_. The morphism is often called the *evaluation function* or the structure map. Think of the functor `F` as forming expressions and the morphism as evaluating them.

Here's the Haskell definition of an F-algebra:

```hs
type Algebra f a = f a -> a
```

It identifies the algebra with its evaluation function.

In the monoid example, the functor in question is:

```hs
data MonF a = MEmpty | MAppend a a
```

This is Haskell for `1 + a × a`.

A ring would be defined using the following functor:

```hs
data RingF a = RZero
             | ROne
             | RAdd a a
             | RMul a a
             | RNeg a
```

which is Haskell for `1 + 1 + a × a + a × a + a`.

An example of a ring is the set of integers. We can choose `Integer` as the carrier type and define the evaluation function as:

```hs
evalZ :: Algebra RingF Integer
evalZ RZero      = 0
evalZ ROne       = 1
evalZ (RAdd m n) = m + n
evalZ (RMul m n) = m * n
evalZ (RNeg n)   = -n
```

There are more F-algebras based on the same functor `RingF`. For instance, polynomials form a ring and so do square matrices.

As you can see, the role of the functor is to generate expressions that can be evaluated using the evaluator of the algebra. So far we've only seen very simple expressions. We are often interested in more elaborate expressions that can be defined using recursion.


## Recursion

One way to generate arbitrary expression trees is to replace the variable `a` inside the functor definition with recursion. For instance, an arbitrary expression in a ring is generated by this tree-like data structure:

```hs
-- after                        -- before
data Expr = RZero               data RingF a = RZero
          | ROne                             | ROne
          | RAdd Expr Expr                   | RAdd a a
          | RMul Expr Expr                   | RMul a a
          | RNeg Expr                        | RNeg a
```

We can replace the original ring evaluator with its recursive version:

```hs
-- original ring evaluator replaced with the recursive version below
evalZ :: Algebra RingF Integer
evalZ RZero      = 0
evalZ ROne       = 1
evalZ (RAdd m n) = m + n
evalZ (RMul m n) = m * n
evalZ (RNeg m)   = - m

-- recursive version
evalZ :: Expr -> Integer
evalZ RZero      = 0
evalZ ROne       = 1
evalZ (RAdd m n) = evalZ m + evalZ n
evalZ (RMul m n) = evalZ m * evalZ n
evalZ (RNeg m)   = - (evalZ m)
```

This is still not very practical, since we are forced to represent all integers as sums of ones and zeros, but it will do in a pinch.

How can we describe expression trees using the language of F-algebras?

>We have to somehow formalize the process of replacing the free type variable in the definition of the endofunctor, recursively, with the result of the replacement.

Imagine doing this in steps. 
First, define a depth-one tree

```hs
type RingF1 a = RingF (RingF a)
```

We are filling the holes in the definition of `RingF` 
with depth-zero trees generated by `RingF a`.

Depth-2 trees are similarly obtained

```hs
type RingF2 a = RingF (RingF (RingF a))

-- which we can also write as
type RingF2 a = RingF (RingF1 a)
```

Continuing this process, we can write a symbolic equation

```hs
type RingFₙ﹢₁ a = RingF (RingFₙ a)

-- or as a fold
type RingFₙ a = RingFⁿ a
```

Conceptually, after repeating this process infinitely many times, we end up with our `Expr`. Notice that `Expr` does not depend on `a`.

The starting point of our journey doesn't matter, we always end up in the same place. This is not true for an arbitrary endofunctor in an arbitrary category, but it is in the category Set. (of course, this is a hand-waving argument, and I'll make it more rigorous later).

Applying an endofunctor infinitely many times produces a **fixpoint**, an object defined as

```hs
Fix f = f (Fix f)

-- which as a newtype looks like
newtype Fix f = Fix (f (Fix f))

-- and with the accesor like
newtype Fix f = Fix { unFix :: f (Fix f) }
```

>The intuition behind this definition is that, since we applied `f` infinitely many times to get `Fix f`, applying it one more time doesn't change anything.

The data ctor `Fix` can be seen as (is) a function:

```hs
Fix :: f (Fix f) -> Fix f
```

The `unFix` accessor peels off a level of functor application

```hs
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
```

The `Fix` and `unFix` are the inverse of each other.


## Category of F-Algebras

Here's the oldest trick in the book: whenever you come up with a way of constructing some new objects, see if they form a category. Not surprisingly,
> algebras over a given endofunctor `F` form a category of F-Algebras.

>Objects in that category are algebras - pairs consisting of a carrier object `a` and a morphism `F a -> a`, both from the original category C.


To complete the picture, we have to define morphisms in the category of F-algebras.

A morphism must map an algebra `(a, f)` to another algebra `(b, g)`.

```hs
m : (a, f) -> (b, g)

f : F a -> a
g : F b -> b

m : a -> b       -- in the original category C
F m : F a -> F b -- in the category of F-Algebras
```

We'll define it as a morphism `m` that maps the carrier types - it goes from `a` to `b` in the original category.

Not any morphism will do: we want it to be compatible with the two evaluators (these structure-preserving morphism are *homomorphism*).

Here's how you define a homomorphism of F-algebras:

First, notice that we can lift `m : a -> b` to the mapping 
`F m : F a -> F b`

and we can then follow it with `g` to get to `b`.   
Equivalently, we can use `f` to go from `F a` to `a`, 
and then follow it with `m`. 

```
F a -------------------> F b
|          F m           |
|                        |
| f                    g |
|                        |
↓           m            ↓
a ---------------------> b
```

We want the two paths to be equal: `g ∘ F m = m ∘ f`


It's easy to convince yourself that this is indeed a category (hint: identity morphisms from _C_ work just fine, and a composition of homomorphisms is a homomorphism).


## Initial algebra

An initial object in the category of F-algebras (if it exists) is called the **initial algebra**.

Let's call the carrier of this initial algebra `i`, 
and its evaluator `j : F i -> i`. 

It turns out that `j`, the evaluator of the initial algebra, is an isomorphism. This result is known as **Lambek's theorem**.

The proof relies on the definition of the initial object, 
which requires that there be a unique homomorphism `m` 
from it to any other F-algebra. 

Since `m` is a homomorphism, the following diagram must commute:

```
F (F a) -------------> F a
|            F m         |
|                        |
| j                    i |
|                        |
|                        |
↓             m          ↓
F a -------------------> a
```



Now let's construct an algebra whose carrier is `F i`.

The evaluator of such an algebra must be a morphism from `F (F i)` to `F i`. 
We can easily construct such an evaluator simply by lifting `j`:

`F j : F (F i) -> F i`


Because `(i, j)` is the initial algebra, there must be a unique homomorphism `m` from it to `(F i, F j)`. 

The following diagram must commute:


[![alg3a](https://bartoszmilewski.files.wordpress.com/2017/02/alg3a.png?w=510)](https://bartoszmilewski.files.wordpress.com/2017/02/alg3a.png)

But we also have this trivially commuting diagram (both paths are the same!):

[![alg3](https://bartoszmilewski.files.wordpress.com/2017/02/alg3.png?w=510)](https://bartoszmilewski.files.wordpress.com/2017/02/alg3.png)


which can be interpreted as showing that `j` is a homomorphism of algebras, mapping `(F i, F j)` to `(i, j)`. We can glue these two diagrams together to get:

[![alg4](https://bartoszmilewski.files.wordpress.com/2017/02/alg4.png?w=300&h=132)](https://bartoszmilewski.files.wordpress.com/2017/02/alg4.png)


This diagram may, in turn, be interpreted as showing that `j ∘ m` is a homomorphism of algebras. Only in this case the two algebras are the same. Moreover, because `(i, j)` is initial, there can only be one homomorphism from it to itself, and that's the identity morphism `idi` - which we know is a homomorphism of algebras. Therefore `j ∘ m = idi`. Using this fact and the commuting property of the left diagram we can prove that `m ∘ j = idFi`. This shows that `m` is the inverse of `j` and therefore `j` is an isomorphism between `F i` and `i`:

`F i ≅ i`

But that is just saying that `i` is a fixed point of `F`. That's the formal proof behind the original hand-waving argument.

Back to Haskell: We recognize `i` as our `Fix f`, `j` as our constructor `Fix`, and its inverse as `unFix`. The isomorphism in Lambek's theorem tells us that, in order to get the initial algebra, we take the functor `f` and replace its argument `a` with `Fix f`. We also see why the fixed point does not depend on `a`.


### Natural Numbers

Natural numbers can also be defined as an F-algebra. The starting point is the pair of morphisms:

zero :: 1 -> N
succ :: N -> N

The first one picks the zero, and the second one maps all numbers to their successors. As before, we can combine the two into one:

1 + N -> N

The left hand side defines a functor which, in Haskell, can be written like this:

data NatF a = ZeroF | SuccF a

The fixed point of this functor (the initial algebra that it generates) can be encoded in Haskell as:

data Nat = Zero | Succ Nat

A natural number is either zero or a successor of another number. This is known as the Peano representation for natural numbers.

## Catamorphisms

Let's rewrite the initiality condition using Haskell notation. We call the initial algebra `Fix f`. Its evaluator is the contructor `Fix`. There is a unique morphism `m` from the initial algebra to any other algebra over the same functor. Let's pick an algebra whose carrier is `a` and the evaluator is `alg`.


By the way, notice what `m` is: It's an evaluator for the fixed point, an evaluator for the whole recursive expression tree. Let's find a general way of implementing it.

Lambek's theorem tells us that the constructor `Fix` is an isomorphism. We called its inverse `unFix`. We can therefore flip one arrow in this diagram to get 


...

Let's write down the commutation condition for this diagram:

`m = alg . fmap m . unFix`

We can interpret this equation as a recursive definition of `m`. The recursion is bound to terminate for any finite tree created using the functor `f`. We can see that by noticing that `fmap m` operates underneath the top layer of the functor `f`. In other words, it works on the children of the original tree. The children are always one level shallower than the original tree.

Here's what happens when we apply `m` to a tree constructed using `Fix f`. The action of `unFix` peels off the constructor, exposing the top level of the tree. We then apply `m` to all the children of the top node. This produces results of type `a`. Finally, we combine those results by applying the non-recursive evaluator `alg`. The key point is that our evaluator `alg` is a simple non-recursive function.

Since we can do this for any algebra `alg`, it makes sense to define a higher order function that takes the algebra as a parameter and gives us the function we called `m`. This higher order function is called a catamorphism:

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

Let's see an example of that. Take the functor that defines natural numbers:

data NatF a = ZeroF | SuccF a

Let's pick `(Int, Int)` as the carrier type and define our algebra as:

fib :: NatF (Int, Int) -> (Int, Int)
fib ZeroF = (1, 1)
fib (SuccF (m, n)) = (n, m + n)

You can easily convince yourself that the catamorphism for this algebra, `cata fib`, calculates Fibonacci numbers.

In general, an algebra for `NatF` defines a recurrence relation: the value of the current element in terms of the previous element. A catamorphism then evaluates the n-th element of that sequence.

## Folds


A list of `e` is the initial algebra of the following functor:

data ListF e a = NilF | ConsF e a

Indeed, replacing the variable `a` with the result of recursion, which we'll call `List e`, we get:

data List e = Nil | Cons e (List e)

An algebra for a list functor picks a particular carrier type and defines a function that does pattern matching on the two constructors. Its value for `NilF` tells us how to evaluate an empty list, and its value for `ConsF` tells us how to combine the current element with the previously accumulated value.

For instance, here's an algebra that can be used to calculate the length of a list (the carrier type is `Int`):

lenAlg :: ListF e Int -> Int
lenAlg (ConsF e n) = n + 1
lenAlg NilF = 0

Indeed, the resulting catamorphism `cata lenAlg` calculates the length of a list. Notice that the evaluator is a combination of (1) a function that takes a list element and an accumulator and returns a new accumulator, and (2) a starting value, here zero. The type of the value and the type of the accumulator are given by the carrier type.

Compare this to the traditional Haskell definition:

length = foldr (\\e n -> n + 1) 0

The two arguments to `foldr` are exactly the two components of the algebra.

Let's try another example:

sumAlg :: ListF Double Double -> Double
sumAlg (ConsF e s) = e + s
sumAlg NilF = 0.0

Again, compare this with:

sum = foldr (\\e s -> e + s) 0.0

As you can see, `foldr` is just a convenient specialization of a catamorphism to lists.

## Coalgebras

As usual, we have a dual construction of an F-coagebra, where the direction of the morphism is reversed:

a -> F a

Coalgebras for a given functor also form a category, with homomorphisms preserving the coalgebraic structure. The terminal object `(t, u)` in that category is called the terminal (or final) coalgebra. For every other algebra `(a, f)` there is a unique homomorphism `m` that makes the following diagram commute:

[![alg7](https://bartoszmilewski.files.wordpress.com/2017/02/alg7.png?w=510)](https://bartoszmilewski.files.wordpress.com/2017/02/alg7.png)

A terminal colagebra is a fixed point of the functor, in the sense that the morphism `u :: t -> F t` is an isomorphism (Lambek's theorem for coalgebras):

F t ≅ t

A terminal coalgebra is usually interpreted in programming as a recipe for generating (possibly infinite) data structures or transition systems.

Just like a catamorphism can be used to evaluate an initial algebra, an anamorphism can be used to coevaluate a terminal coalgebra:

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

A canonical example of a coalgebra is based on a functor whose fixed point is an infinite stream of elements of type `e`. This is the functor:

data StreamF e a = StreamF e a
  deriving Functor

and this is its fixed point:

data Stream e = Stream e (Stream e)

A coalgebra for `StreamF e` is a function that takes the seed of type `a` and produces a pair (`StreamF` is a fancy name for a pair) consisting of an element and the next seed.

You can easily generate simple examples of coalgebras that produce infinite sequences, like the list of squares, or reciprocals.

A more interesting example is a coalgebra that produces a list of primes. The trick is to use an infinite list as a carrier. Our starting seed will be the list `[2..]`. The next seed will be the tail of this list with all multiples of 2 removed. It's a list of odd numbers starting with 3. In the next step, we'll take the tail of this list and remove all multiples of 3, and so on. You might recognize the makings of the sieve of Eratosthenes. This coalgebra is implemented by the following function:

era :: \[Int\] -> StreamF Int \[Int\]
era (p : ns) = StreamF p (filter (notdiv p) ns)
    where notdiv p n = n \`mod\` p /= 0

The anamorphism for this coalgebra generates the list of primes:

primes = ana era \[2..\]

A stream is an infinite list, so it should be possible to convert it to a Haskell list. To do that, we can use the same functor `StreamF` to form an algebra, and we can run a catamorphism over it. For instance, this is a catamorphism that converts a stream to a list:

toListC :: Fix (StreamF e) -> \[e\]
toListC = cata al
   where al :: StreamF e \[e\] -> \[e\]
         al (StreamF e a) = e : a

Here, the same fixed point is simultaneously an initial algebra and a terminal coalgebra for the same endofunctor. It's not always like this, in an arbitrary category. In general, an endofunctor may have many (or no) fixed points. The initial algebra is the so called least fixed point, and the terminal coalgebra is the greatest fixed point. In Haskell, though, both are defined by the same formula, and they coincide.

The anamorphism for lists is called unfold. To create finite lists, the functor is modified to produce a `Maybe` pair:

unfoldr :: (b -> Maybe (a, b)) -> b -> \[a\]

The value of `Nothing` will terminate the generation of the list.

An interesting case of a coalgebra is related to lenses. A lens can be represented as a pair of a getter and a setter:

set :: a -> s -> a
get :: a -> s

Here, `a` is usually some product data type with a field of type `s`. The getter retrieves the value of that field and the setter replaces this field with a new value. These two functions can be combined into one:

a -> (s, s -> a)

We can rewrite this function further as:

a -> Store s a

where we have defined a functor:

data Store s a = Store (s -> a) s

Notice that this is not a simple algebraic functor constructed from sums of products. It involves an exponential `as`.

A lens is a coalgebra for this functor with the carrier type `a`. We've seen before that `Store s` is also a comonad. It turns out that a well-behaved lens corresponds to a coalgebra that is compatible with the comonad structure. We'll talk about this in the next section.


## Exercises

1. Implement the evaluation function for a ring of polynomials of one variable.

You can represent a polynomial as a list of coefficients in front of powers of `x`. For instance, `4x2-1` would be represented as (starting with the zeroth power) `[-1, 0, 4]`.

2. Generalize the previous construction to polynomials of many independent variables, like `x2y-3y3z`.

3. Implement the algebra for the ring of 2×2 matrices.

4. Define a coalgebra whose anamorphism produces a list of squares of natural numbers.

5. Use `unfoldr` to generate a list of the first `n` primes.

## Next

[Algebras for Monads](https://bartoszmilewski.com/2017/03/14/algebras-for-monads/)
