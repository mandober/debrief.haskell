# Bottom

https://wiki.haskell.org/Bottom

## Bottom
Haskell is not a total language: at any point, a computation may fail due to some error (e.g. division by zero), non-termination (e.g. infinite loop), or even by deliberately throwing an exception because there is no sensible value to return. The last happens too frequently - many functions from the Haskell's stdlib may panic; e.g. what can the ubiqutous `head` function do given the empty list? It can but explode.

The fact that functions can panic (throw, explode) is made possible by the fact that every type in Haskell has a special term (value) available which it can return (throw) in such situations. This term is called *bottom*, and it originates from logic, where it is denoted by `⊥`, marking contradiction. More precisely, every *lifted type* in Haskell can panic because it has bottom as one of its values. Unlifted types cannot panic. An unlifted type, like `ℕ`, becomes lifted by extending it to include bottom, `ℕ ⋃ {⊥}`. So, each everyday type in Haskell includes the special value bottom. Bottom proliferates deep - almost any construction has a bottom as a value, i.e. it is not the case that there is only one bottom value associated with ℕ - there are loads.

```hs
-- definition of naturals
data ℕ where
  Z :: ℕ
  S :: ℕ → ℕ

-- (well-formed) naturals:
n0,n1,n2 :: ℕ
n0 = Z
n1 = S Z
n2 = S (S Z)

-- but these are also (ill-formed) naturals:
x1,x2,x3,inf :: ℕ
x1 = x1             -- bottom, infinite loop
x2 = undefined      -- bottom, `undefined`
x3 = error "blah"   -- bottom, by calling `error`
inf = S inf         -- bottom, infinity: S (S (S (S (…
```

Since Hakell is not a total language, types may have ill-formed terms. In type theory, types have inference (proof) rules which govern how a type is formed, introduced, eliminated, computed, etc. There are also *η-rules* that make sure that an introduction followed by immediate elimination gives the expected type. For example, a pair (i.e. product) has one introduction rule and two elimination rules, called projections: the first projection, `π₁`, returns the first component of a pair, `π₂` the second. Given a pair `p`, the η rule makes sure that:

    (π₁ p, π₂ p) = p

The η rules assert that no strange term is lurking in the type; that all terms are well-formed. This would fail in Haskell since a type can indeed have degenarate terms. In a total language like Agda this is not possible - and there is no bottom either. Proofs perfomed in Agda do not have to fear non-termination: it is not present and the programmer has to make sure of that. Agda also helps by checking that recursive calls are always made with an argument that is somehow "smaller", so the process is guranteed to terminate (and there are bunch of other rules that make sure of that).





The term *bottom* refers to a computation which never completes successfully, aka *divergent computation*. Such computations include failures due to error (e.g. division by zero), or non-termination (e.g. going into an infinite loop).

The symbol for bottom, `⊥`, is takes from logic, where it denotes contradiction, which has similar interpretation to a divergent computation: a computation that "bottoms-out" is similar to deriving a contradiction during a logical proof.

Bottom is a member of any and all types - whatever type we have we can always return a bottom - e.g. as `undefined` - and the type checker will be satisfied (although when executed, such computation will definitely fail).

Types without bottom are called *unlifted types*, and these are the (proper) primitive types in Haskell such as `Int#` and `Float#`. However, functions involving unlifted types then have no means of expressing failure. Sure, a function that works with `Int#`, but actually only returns natural numbers, can signal failure in a C-like manner by returning -1, but such *in-band error signaling* is fragile and, after all, not applicable in general. The unlifted type of integers is much like the math set `ℤ`. In math, and in Haskell, as well such type can be lifted by augmenting it with a bottom.



(e.g. division that is concerned with zero as the divisor, but in fact, any other function as well

) that uses unlifted types cannot express failure






If it were not, the compiler could solve the halting problem and statically determine whether any computation terminated. 

Note that some languages with dependent type systems (Agda, Idris, Epigram) can statically enforce termination, based on the type for particular programs, such as those using induction.

Bottom can be expressed in Haskell thus:
Indeed, the Prelude exports a function

```hs
bottom = bottom

bottom = error "Non-terminating computation!"

undefined = error "Prelude.undefined"
```

Other implementations of Haskell, such as Gofer, defined bottom as:

undefined | False = undefined
The type of bottom is arbitrary, and defaults to the most general type:

undefined :: a
As bottom is an inhabitant of every type (though with some caveats concerning types of unboxed kind), bottoms can be used wherever a value of that type would be. This can be useful in a number of circumstances:

-- For leaving a todo in your program to come back to later:
foo = undefined

-- When dispatching to a type class instance:
print (sizeOf (undefined :: Int))

-- When using laziness:

print (head (1 : undefined))

## The Error Function

Haskell has a built-in function called `error`

```hs
error :: String -> a

-- more explicitly:
error :: forall a. String -> a
```

The type of `error` is the function type `String -> a`, which is very strange: it says that the 'error' returns a value of a polymorphic type (`a`) about which it knows nothing - since it never receives a value of that type as an argument!

The `error` function is actually polymorphic in the output (return) value!

>The 'error' is somewhat similar to the `id` function:

```hs
id :: a -> a
id a = a

-- more explicitly:
id :: forall a. a -> a
```

but the `id` first receives a value (of some type `a`) as an arguemnt before it returns that same value.

>The `id` is polymorphic is the input value.

What happens with the `error` function is that it reduces to the *bottom*.

Semantically that is exactly what value is always returned by the `error` because all diverging computations have value `⊥` - they all "bottom-out".

However, before it causes a panic and the program is aborted, the `error` function will print the supplied message to the stderr for diagnostic purposes, justifying the use of `String` type for the argument.

The `error` function is useful when we want to deliberatly terminate the program but also print a message about what has gone so terribly wrong.

Although the work to exile them from the `Prelude` is undergoing, many *partial functions* are still there, e.g. this is the actual definition of `head` there:

```hs
head (x:xs) = x
head  []    = error "head{PreludeList}: head []"
```
