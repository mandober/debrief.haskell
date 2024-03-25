# Functions, values and custom data types

The relations between values, functions and data types (data structures).

- butt naked values of a butt naked type (BNT)
- coercible containerized type (CCT)
- newtype as a light wrapper, a ≅ T a, for single-data-ctored types
- we discuss a particular value but only mention its type: this is because a value is abstracted by its type. All values of that type are "the same" for our purposes. No point in singling out one value, so we talk about them via their type. They are all characterized by the type, so we talk about the type, but mean any of the inhabiting values. Using the type as the mediator for a value (one, any of its values) is so convenient and common that avoiding to overload the term "type" cannot be avoided.


The most general type is (yikes) `∀a. a`, but that bears a discussion on the topic from whose aspect is it the most general. It ain't from the callee's aspect, fo sho. The callee is the function definition itself and from its perspective it can do shit; all types are decided by the caller, from whose perspective the `a` can really stand fro any type.

```hs
newtype ID       a   = ID       a               -- a -> a
data    Function a b = Function a b             -- a -> b
data    Pair     a b = Pair     a b             -- a -> b
newtype Const    a b = Const    a               -- a -> b -> a
newtype Konst    a b = Konst (a -> b -> a)      -- a -> b -> a
```


## Function vs data type declaration

Standalone function (SAF) vs function obtained through a data type declaration (DTD).

Hell. There's nothing wrong in dealing with a value directly since many values have (or can be massaged into the appropriate) shape that is expected as a target for a class instance. Instances are intended for all shapes of type ctors, from bare, across partially applied to the fully saturated type ctors, but type ctors they must be. So, not just any old type (i.e. its inhabiting values) is ready to receive the blessing of a class membership. It must have a type-constructory sort of shape. If it doesn't, or if it was already used for implementing another class, no problem, it can still apply for any class membership just as soon as it is wrapped in a newtype. And a newtype certainly has a type-constructory sort of shape, although it comes with a cost that can inflict sharp protruding pain at times, a 

The Essence of Functional Programming 1992 Philip Wadler




## The Point

<a ≅ Val a>


Wrapping a value of type `a` into a quasi-container using a newtype means a lot of un/re/wrapping ahead. However, aside from the wrapping business, the original, bare, value and its newtyped version are isomorphic, in fact, they're pretty much the same since they can be coerced to and fo from and into each other.

A plain butt-naked value, `a` is isomorphic to its containerized version, i.e. a newtype-wrapped version, sans the unavoidable data ctor nuisance and the avoidable accessor function.




## rant 342

The function application, `f x`, is the act of applying a function "f" to the argument "x", denoted, again, by `f x`. Writing out such an expression is recognized as function application and the corresponding machinery around it is triggered in the language. The application is thus implemented in the language itself, in the compiler, and is not user-definable (UD). All we can do is indirect it by writing some permutations on the theme, such as `apply`, `$`, `$!` (not cosher), `&`, `seq` (non-cosher).

The moving parts that fuel the permutations are the order of the (two) arguments, name (a cryptic alphabetic or an intuitive-looking symbolic label; or no name at all! use blank unicode, bet there's plenty), pattern-matching with strictness considerations, type-ranking, foralling it deeply; seems we have quite a lot of factors after all.

```hs
apply :: ∀a. (a -> b) -> a -> b
apply f x = f x
```


## rant 241

A function that has a signature `∀a. a -> a` has a single sensible definition - that of the identity function. The definition (implementation) although trivial is very much required; a signature cannot float around unpaired with an appropriate implementation, unless... Surely, it cannot, can it? Sure, it can! It just needs to skew its form a little. The function in question, that is.

```hs
id :: ∀a. (a -> a)
id x = x
```

For example, the `id` function, as already said, cannot be half-assed (upper-assed) by merely stating the signature, but it can be lower-assed by merely stating its implementation. That is true but solely on the function-function level. However, on the datatype-function level, the portions are swapped, so a signature is enough to define a function completely. In fact, you couldn't write the implementation even in despair.

```hs
id :: ∀a. (a -> a)
id x = x
```


## rant 326 (delete later)

Variables do not really exist in Haskell, that is, we can talk about them in the way we're accustomed to, but they are merely nullary functions. Since Haskell doesn't have explicit syntax for function application, invoking a nullary function called "f" is denoted by `f`, which is identical to merely stating it as a name, e.g. to passing it to another function as an arg, `f`.

In math amd most PLs, invoking a nullary function called "f" is done by following its name with a pair of parens, denoted by `f()`. Usually, the spacing is insignificant, so feel free to space thing out, especially in JS when decorating all functions with currying facilities; after being curryied, functions may take one arg at the time, which with necessary parens may look very noisy.

## rant

In Haskell, the expression, `f ()`, is not a call to a nullary but to a unary function named `f` with the argument `()` (unit, empty tuple). Since the unit value, `()`, is (almost) the sole inhabitant of the unit type , `() :: ()`, not only is the function not nullary, it is unary and it has a completely different and distinct signature.


## rant

values, functions and data types 




## Intro rant

A plain (unstructured) but polymorphic value of type `a`, explicitly denoted by `∀a.a`, has no sensible inhabitants. Apart from the usual bottoms in the form of
- self-reference that causes an infinite loop
- self-typing
- `undefined`
- `error`

that find their way into each and every type anyway; although there's just one bottom value, `⊥`, it still has several forms. In its most tangible form, i.e. as `undefined` it has the type `a`, more precisely, `∀a. a`, meaning it can shape-shift into any type (that the caller supplies).

```hs
value1 :: ∀a. a

-- 1: undefined
value1 = undefined

-- 2 self-typing
value2 = value2a :: a
value2@ = value2b @a

-- 3
value3 = error "why"
-- 4
value4 = value4
-- etc.
valuetc = valuetc undefined
```

Actually and interestingly, there is another term that conforms with `value` - it is the value `value :: a` itself, a recursive reference to self.
