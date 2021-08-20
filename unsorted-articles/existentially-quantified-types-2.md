# Haskell/Existentially quantified types

> Existential types, or 'existentials' for short, are a way of 'squashing' a group of types into one, single type.

Existential types, or 'existentials' for short, are a way of 'squashing' a group of types into one, single type.

Existentials are part of GHC's _type system extensions_. They aren't part of Haskell98, and as such you'll have to either compile any code that contains them with an extra command-line parameter of `-XExistentialQuantification`, or put `{-# LANGUAGE ExistentialQuantification #-}` at the top of your sources that use existentials.

The `forall` keyword
--------------------

The `forall` keyword is used to explicitly bring fresh type variables into scope. For example, consider something you've innocuously seen written a hundred times so far:

**Example:** A polymorphic function

map :: (a -> b) -> \[a\] -> \[b\]

But what are these `a` and `b`? Well, they're type variables, you answer. The compiler sees that they begin with a lowercase letter and as such allows any type to fill that role. Another way of putting this is that those variables are 'universally quantified'. If you've studied formal logic, you will have undoubtedly come across the quantifiers: 'for all' (or ![\forall ](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/bfc1a1a9c4c0f8d5df989c98aa2773ed657c5937.svg)) and 'exists' (or ![\exists ](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/77ed842b6b90b2fdd825320cf8e5265fa937b583.svg)). They 'quantify' whatever comes after them: for example, ![\exists x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/ab833914405cde960b3b9af3feaa9e4fef96ffa9.svg) means that whatever follows is true for at least one value of _x_. ![\forall x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/1a3fa2fb002baecbc5038bd3dd42bab57448b315.svg) means that what follows is true for every possible value of _x_ you can imagine. For example, ![\forall x,\,x^{2}\geq 0](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/8d5b2a3aad3d98e8593190693a3d1c4896ce415b.svg) and ![\exists x,\,x^{3}=27](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/b22ef4d5264f643e3de230f7a46869dfb959624c.svg).

The `forall` keyword quantifies _types_ in a similar way. We would rewrite `map`'s type as follows:

**Example:** Explicitly quantifying the type variables

map :: forall a b. (a -> b) -> \[a\] -> \[b\]

So we see that for any combination of types `a` and `b` we can imagine, `map` takes the type `(a -> b) -> [a] -> [b]`. For example, we might choose `a = Int` and `b = String`. Then it's valid to say that `map` has the type `(Int -> String) -> [Int] -> [String]`. Here we are _instantiating_ the general type of `map` to a more specific type.

However, in Haskell, any introduction of a lowercase type parameter implicitly begins with a `forall` keyword, so those two previous type declarations for `map` are equivalent, as are the declarations below:

**Example:** Two equivalent type statements

id :: a -> a
id :: forall a . a -> a

What makes life really interesting and the `forall` so useful is that you can apply additional constraints on the type variables it introduces. Such constraints, ![P(x)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/89833156eff2c51bfb8750db3306a0544ce34e14.svg), serve to guarantee certain properties of the type variable, ![x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/87f9e315fd7e2ba406057a97300593c4802b53e4.svg), as a kind of ad-hoc interface restriction, (similar to ![{\displaystyle \exists x,P(x)}](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/9afd4484f78365d7aa62d707902106f905562701.svg) or ![{\displaystyle \forall x,P(x)}](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/0abfc36810fe48436e03a79d2fd968772d7972df.svg) stipulations).

Let's dive right into the deep end of this by seeing an example of the power of existential types in action.

Example: heterogeneous lists
----------------------------

The premise behind Haskell's type class system is grouping types that all share a common property. So if you know a type that is a member of some class `C`, you know certain things about that type. For example, `Int` is a member of class `Eq`, so we know that elements of `Int` can be compared for equality.

Suppose we have a group of values. We don't know if they are all the same type, but we do know they are all members of some class (and, by extension, that all the values have a certain property). It might be useful to throw all these values into a list. We can't do this normally because lists elements must be of the same type (homogeneous with respect to types). However, existential types allow us to loosen this requirement by defining a 'type hider' or 'type box':

**Example:** Constructing a heterogeneous list

 data ShowBox = forall s. Show s => SB s
 
 heteroList :: \[ShowBox\]
 heteroList = \[SB (), SB 5, SB True\]

We won't explain precisely what we mean by that data type definition, but its meaning should be clear to your intuition. The important thing is that we're calling the constructor on three values of different types, `[SB (), SB 5, SB True]`, yet we are able to place them all into a single list, so we must somehow have the same type for each one. Essentially, yes. This is because our use of the `forall` keyword gives our constructor the type `SB :: forall s. Show s => s -> ShowBox`. If we were now writing a function to which we intend to pass `heteroList`, we couldn't apply a function such as `not` to the values inside the `SB` because their type might not be `Bool`. But we do know something about each of the elements: they can be converted to a string via `show`. In fact, that's pretty much the only thing we know about them.

**Example:** Using our heterogeneous list

 instance Show ShowBox where
  show (SB s) = show s        -- (\*) see the comment in the text below
 
 f :: \[ShowBox\] -> IO ()
 f xs = mapM\_ print xs

 main = f heteroList

Let's expand on this a bit more. In the definition of `show` for `ShowBox` – the line marked with `(*) see the comment in the text below` – we don't know the type of `s`. But as we mentioned, we _do_ know that the type is an instance of Show due to the constraint on the `SB` constructor. Therefore, it's legal to use the function `show` on `s`, as seen in the right-hand side of the function definition.

As for `f`, recall the type of print:

**Example:** Types of the functions involved

 print :: Show s => s -> IO () -- print x = putStrLn (show x)
 mapM\_ :: (a -> m b) -> \[a\] -> m ()
 mapM\_ print :: Show s => \[s\] -> IO ()

As we just declared `ShowBox` an instance of `Show`, we can print the values in the list.

A Further Explanation
---------------------

One way to think about `forall` is to think about types as a set of possible values. For example, Bool is the set {True, False, ⊥} (remember that bottom, ⊥, is a member of every type!), Integer is the set of integers (and bottom), String is the set of all possible strings (and bottom), and so on. `forall` serves as a way to assert a commonality or _intersection_ of the specified types (i.e. sets of values). For example, `forall a. a` is the intersection of all types. This subset turns out to be the set whose sole element is bottom, {⊥}, since it is an implicit value in every type. That is, the type whose only available value is bottom. However, since every Haskell type includes bottom, {⊥}, this quantification in fact stipulates all Haskell types. However, the only permissible operations on it are those available to a type whose only element is bottom.

A few more examples:

1.  The list, `[forall a. a]`, is the type of a list whose elements all have the type `forall a. a`, i.e. a list of bottoms.
2.  The list, `[forall a. Show a => a]`, is the type of a list whose elements all have the type `forall a. Show a => a`. The Show class constraint requires the possible types to also be a member of the class, Show. However, ⊥ is still the only value common to all these types, so this too is a list of bottoms.
3.  The list, `[forall a. Num a => a]`, requires each element to be a member of the class, Num. Consequently, the possible values include numeric literals, which have the specific type, `forall a. Num a => a`, as well as bottom.
4.  `forall a. [a]` is the type of the list whose elements all have the same type `a`. Since we cannot presume any particular type at all, this too is a list of bottoms.

We see that most intersections over types just lead to bottoms because types generally don't have any values in common and so presumptions cannot be made about a union of their values.

However, recall that in the last section, we developed a heterogeneous list using a 'type hider'. This 'type hider' functions as a wrapper type which guarantees certain facilities by implying a predicate or constraint on the permissible types. In that case it was that they must be a member of the type class `Show`. In general, that seems to be the purpose of `forall`, to impose type constraint on the permissible types within a type declaration and thereby guaranteeing certain facilities with such types.

Let's declare one.

**Example:** An existential datatype

This means that:

**Example:** This defines a polymorphic constructor, or a family of constructors for T

MkT :: forall a. (a -> T)

So we can pass any type, `a`, we want to `MkT` and it will create a T. So what happens when we deconstruct a `T` value with pattern matching...?

**Example:** Pattern matching on our existential constructor

 foo (MkT x) = ... -- what is the type of x?

As we've just stated, `x` could be of any type. That means it's a member of some arbitrary type, so has the type `forall a. a`. In other words the set whose only available value is bottom, ⊥.

However, we can make a heterogeneous list:

**Example:** Constructing the hetereogeneous list

 heteroList = \[MkT 5, MkT (), MkT True, MkT map\]

Of course, when we pattern match on `heteroList` we cannot presume any features about its elements[\[1\]](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/A/Haskell/Existentially_quantified_types#cite_note-1). So technically, we can't do anything _useful_ with its elements, except reduce them to WHNF.[\[2\]](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/A/Haskell/Existentially_quantified_types#cite_note-2) However, if we introduce class constraints:

**Example:** A new existential data type, with a class constraint

 data T' = forall a. Show a => MkT' a

The class constraint serves to limit the types we are intersecting over, such that we now have values inside a `T'` which are elements of some arbitrary type _that are members of Show_. The implication of this is that we can apply `show` to a value of type `a` upon deconstruction. It doesn't matter exactly which type it turns out to be.

**Example:** Using our new heterogenous setup

 heteroList' = \[MkT' 5, MkT' (), MkT' True, MkT' "Sartre"\]
 main = mapM\_ (\\(MkT' x) -> print x) heteroList'

 {- prints:
 5
 ()
 True
 "Sartre"
 -}

To summarize, the interaction of the universal quantifier with data types produces a qualified subset of types guaranteeing certain facilities as described by one or more class constraints.

Example: `runST`
----------------

One monad that you may not have come across so far is the ST monad. This is essentially a more powerful version of the `State` monad: it has a much more complicated structure and involves some more advanced topics. It was originally written to provide Haskell with IO. As we mentioned in the [Understanding monads](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/A/Haskell/Understanding_monads "Haskell/Understanding monads") chapter, IO is basically just a State monad with an environment of all the information about the real world. In fact, inside GHC at least, ST is used, and the environment is a type called `RealWorld`.

To get out of the State monad, you can use `runState`. The analogous function for ST is called `runST`, and it has a rather particular type:

**Example:** The `runST` function

runST :: forall a. (forall s. ST s a) -> a

This is actually an example of a more complicated language feature called rank-2 polymorphism, which we don't go into detail here. It's important to notice that there is no parameter for the initial state. Indeed, ST uses a different notion of state to State; while State allows you to `get` and `put` the current state, ST provides an interface to _references_. You create references, which have type `STRef`, with `newSTRef :: a -> ST s (STRef s a)`, providing an initial value, then you can use `readSTRef :: STRef s a -> ST s a` and `writeSTRef :: STRef s a -> a -> ST s ()` to manipulate them. As such, the internal environment of a ST computation is not one specific value, but a mapping from references to values. Therefore, you don't need to provide an initial state to runST, as the initial state is just the empty mapping containing no references.

However, things aren't quite as simple as this. What stops you creating a reference in one ST computation, then using it in another? We don't want to allow this because (for reasons of thread-safety) no ST computation should be allowed to assume that the initial internal environment contains any specific references. More concretely, we want the following code to be invalid:

**Example:** Bad ST code

 let v = runST (newSTRef True)
 in runST (readSTRef v)

What would prevent this? The effect of the rank-2 polymorphism in `runST`'s type is to _constrain the scope of the type variable `s`_ to be within the first parameter. In other words, if the type variable `s` appears in the first parameter it cannot also appear in the second. Let's take a look at how exactly this is done. Say we have some code like the following:

**Example:** Briefer bad ST code

... runST (newSTRef True) ...

The compiler tries to fit the types together:

**Example:** The compiler's typechecking stage

newSTRef True :: forall s. ST s (STRef s Bool)
runST :: forall a. (forall s. ST s a) -> a
together, (forall s. ST s (STRef s Bool)) -> STRef s Bool

The importance of the `forall` in the first bracket is that we can change the name of the `s`. That is, we could write:

**Example:** A type mismatch!

together, (forall s'. ST s' (STRef s' Bool)) -> STRef s Bool

This makes sense: in mathematics, saying ![\forall x.x>5](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/5352d4734bff5e984c62df1aed1cfd2998c134ab.svg) is precisely the same as saying ![\forall y.y>5](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/I/m/c76cabd64d671775d2d0a8b7ed9ef6b8d3484ba8.svg); you're just giving the variable a different label. However, we have a problem with our above code. Notice that as the `forall` does _not_ scope over the return type of `runST`, we don't rename the `s` there as well. But suddenly, we've got a type mismatch! The result type of the ST computation in the first parameter must match the result type of `runST`, but now it doesn't!

The key feature of the existential is that it allows the compiler to generalise the type of the state in the first parameter, and so the result type cannot depend on it. This neatly sidesteps our dependence problems, and 'compartmentalises' each call to `runST` into its own little heap, with references not being able to be shared between different calls.

Quantified Types as Products and Sums
-------------------------------------

A universally quantified type may be interpreted as an infinite product of types. For instance, a polymorphic function like:

**Example:** Identity function

 id :: forall a. a -> a
 id a = a

can be understood as a product, or a tuple, of individual functions, one per every possible type `a`. To construct a value of such type, we have to provide all the components of the tuple at once. In case of function types, this is possible because of polymorphism -- one formula generating an infinity of functions.

In case of numeric types, one numeric constant may be used to initialize multiple types at once. For instance, in:

**Example:** Polymorphic value

 x :: forall a. Num a => a
 x = 0 

`x` may be conceptualized as a tuple consisting of an `Int` value, a `Double` value, etc.

Similarly, an existentially quantified type may be interpreted as an infinite sum. For instance,

**Example:** Existential type

 data ShowBox = forall s. Show s => SB s

may be conceptualized as a sum:

**Example:** Sum type

 data ShowBox = SBUnit | SBInt Int | SBBool Bool | SBIntList \[Int\] | ...

To construct a value of this type, we only have to pick one of the constructors. A polymorphic constructor `SB` combines all those constructors into one.

Quantification as a primitive
-----------------------------

Universal quantification is useful for defining data types that aren't already defined. Suppose there was no such thing as pairs built into haskell. Quantification could be used to define them.

{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

newtype Pair a b = Pair (forall c. (a -> b -> c) -> c)

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \\f -> f a b

In GHCi:

 λ> :set -XExistentialQuantification
 λ> :set -XRankNTypes
 λ> newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}
 λ> makePair a b = Pair $ \\f -> f a b
 λ> pair = makePair "a" 'b' 
 
 λ> :t pair
 pair :: Pair \[Char\] Char
 
 λ> runPair pair (\\x y -> x)
 "a"
 
 λ> runPair pair (\\x y -> y)
 'b'

Notes
-----

1.  [↑](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/A/Haskell/Existentially_quantified_types#cite_ref-1) However, we can apply them to functions whose type is `forall a. a -> _R_`, for some arbitrary `_R_`, as these accept values of any type as a parameter. Examples of such functions: `id`, `const k` for any `k`, `seq`
2.  [↑](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/A/Haskell/Existentially_quantified_types#cite_ref-2) because all we know is that they have some arbitrary type.

Further reading
---------------

*   [24 Days of GHC Extensions: Existential Quantification](https://ocharles.org.uk/blog/guest-posts/2014-12-19-existential-quantification.html)
*   GHC's user guide contains [useful information](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#existentially-quantified-data-constructors) on existentials, including the various limitations placed on them (which you should know about).
*   _[Lazy Functional State Threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.3299), by Simon Peyton-Jones and John Launchbury, is a paper which explains more fully the ideas behind ST._


[Source](https://zims-en.kiwix.campusafrica.gos.orange.com/wikibooks_en_all_maxi/A/Haskell/Existentially_quantified_types)