# Existentially quantified data constructors

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/existential_quantification.html

https://downloads.haskell.org/~ghc/9.4.2/docs/html/users_guide/exts/existential_quantification.html


- type declarations
- existential 
- existentially-quantified data constructors
- existentially quantified type variables
- existentially quantified type variables in types


- `ExistentialQuantification`
- desc: Enable liberalised type synonyms
- implies: ExplicitForAll
- since: 6.8.1
- Allow existentially quantified type variables in types


> Existential types allow us to package heterogeneous values together with functions that manipulate them. We can then treat that collection of packages in a uniform manner.

The idea of using existential quantification in data type declarations was suggested by Perry, and implemented in Hope+ (Nigel Perry, *The Implementation of Practical Functional Programming Languages*, PhD Thesis, University of London, 1991). It was later formalised by Laufer and Odersky (*Polymorphic type inference and abstract data types*, TOPLAS, 16(5), pp. 1411-1430, 1994). It's been in Lennart Augustsson's `hbc` Haskell compiler for several years, and proved very useful.

Here's the idea. Consider the declaration:

```hs
data Foo = forall a. MkFoo a (a -> Bool) | Nil

-- The data type Foo has two constructors with types:
MkFoo :: forall a. a -> (a -> Bool) -> Foo
Nil   :: Foo
```

Notice that the type variable `a` in the type of `MkFoo` doesn't appear in the data type itself (on the LHS), which is plain `Foo`. For example, the
following expression is fine:

```hs
[MkFoo 3 even, MkFoo 'c' isUpper] :: [Foo]
```

Here, `MkFoo 3 even` packages an integer with a function `even` that maps an integer to `Bool`; and `MkFoo 'c' isUpper` packages a character with a compatible function. These two things are each of type `Foo` and can be put in a list.

What can we do with a value of type `Foo`? In particular, what happens
when we pattern-match on `MkFoo`?

```hs
f (MkFoo val fn) = ???
```

Since all we know about `val` and `fn` is that they are compatible, the only (useful) thing we can do with them is to apply `fn` to `val` to get a boolean.

```hs
f :: Foo -> Bool
f (MkFoo val fn) = fn val
```

> What existential types allow us to do is package heterogeneous values together with a bunch of functions that manipulate them, and then treat that collection of packages in a uniform manner.

You can express quite a bit of OOP this way.


## Why existential?

What has this to do with *existential quantification*? Simply that `MkFoo` has the (nearly) isomorphic type:

```hs
MkFoo :: (exists a . (a, a -> Bool)) -> Foo
```

But Haskell programmers can safely think of the ordinary *universally quantified* type given above, thereby avoiding adding a new existential quantification construct.


## Existentials and type classes

An easy extension is to allow arbitrary contexts before the constructor.

```hs
data Baz = forall a. Eq   a => Baz1 a a
         | forall b. Show b => Baz2 b (b -> b)

-- The two constructors have the types you'd expect:
Baz1 :: forall a. Eq   a => a -> a -> Baz
Baz2 :: forall b. Show b => b -> (b -> b) -> Baz
```

When pattern matching:
- on Baz1 the matched values can be compared for equality
- on Baz2 the first matched value can be converted to a string   
  (and we can apply a function to it)

```hs
f :: Baz -> String
f (Baz1 p q)  = if p == q then "Yes" else "No"
f (Baz2 v fn) = show (fn v)
```

Operationally, in a dictionary-passing implementation, the constructors `Baz1` and `Baz2` *must store the dictionaries* for `Eq` and `Show` respectively, and *extract them on pattern matching*.


## Record Constructors

GHC allows existentials to be used with records syntax as well.

```hs
data Counter a = forall self. NewCounter
    { _this    :: self
    , _inc     :: self -> self
    , _display :: self -> IO ()
    , tag      :: a
    }
```

Here `tag` is a public field, with a well-typed selector function `tag :: Counter a -> a`.

See "Field selectors and TypeApplications" for a full description of how the types of top-level field selectors are determined:   
https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/field_selectors_and_type_applications.html#field-selectors-and-type-applications

The `self` type is hidden from the outside; any attempt to apply `_this`, `_inc` or `_display` as functions will raise a compile-time error. In other words, *GHC defines a record selector function only for fields whose type does not mention the existentially-quantified variables* (underscores in the field names are there only stilistically; GHC ignores them).

To make use of these hidden fields, we need to create some helper functions:

```hs
inc :: Counter a -> Counter a
inc (NewCounter x i d t) = NewCounter
    { _this = i x, _inc = i, _display = d, tag = t }

display :: Counter a -> IO ()
display NewCounter{ _this = x, _display = d } = d x
```

Now we can define counters with different underlying implementations:

```hs
counterA :: Counter String
counterA = NewCounter
    { _this = 0, _inc = (1+), _display = print, tag = "A" }

counterB :: Counter String
counterB = NewCounter
    { _this = "", _inc = ('#':), _display = putStrLn, tag = "B" }

main = do
    display (inc counterA)         -- prints "1"
    display (inc (inc counterB))   -- prints "##"
```

Record update syntax is supported for existentials (and GADTs): :

```hs
setTag :: Counter a -> a -> Counter a
setTag obj t = obj{ tag = t }
```

> The rule for record update is this: the types of the updated fields may mention only the universally-quantified type variables of the data constructor. For GADTs, the field may mention only types that appear as a simple type-variable argument in the constructor's result type.

For example:

```hs
-- RECORD UPDATES

data T a b where
  T1 { f1 :: a, f2 :: b, f3 :: (b, c) } :: T a b
-- c is existential

upd1 t x = t { f1 = x }
-- OK: upd1 :: T a b -> a' -> T a' b

upd2 t x = t { f3 = x }
-- BAD  f3's type mentions c, which is exist. quant.

data G a b where
  G1 { g1 :: a, g2 :: c } :: G a [c]

upd3 g x = g { g1 = x }
-- OK: upd3 :: G a b -> c -> G c b

upd4 g x = g { g2 = x }
-- BAD: g2's type mentions c which isn't
-- a simple type-var arg in G1's result type
```



## Restrictions

There are several restrictions on the ways in which existentially-quantified constructors can be used.

1. When pattern matching, **each pattern match introduces a new, distinct, type for each existential type variable**. These types cannot be unified with any other type, nor can they escape from the scope of the pattern match.

For example, these fragments are incorrect:

```hs
f1 (MkFoo a f) = a
```

Here, the type bound by `MkFoo` "escapes", because `a` is the result of `f1`. One way to see why this is wrong is to ask what type `f1` has:

```hs
f1 :: Foo -> a -- Weird!
```

What is this `a` in the result type? Clearly we don't mean this:

```hs
f1 :: forall a. Foo -> a   -- Wrong!
```

The original program is just plain wrong. Here's another sort of error:

```hs
f2 (Baz1 a b) (Baz1 p q) = a==q
```

It's ok to say `a==b` or `p==q`, but `a==q` is wrong because it equates the two distinct types arising from the two `Baz1` constructors.


2. You **cannot pattern-match on an existentially quantified constructor in a `let` or `where` group of bindings**.

```hs
-- This is illegal:
f3 x = a==b where { Baz1 a b = x }

-- Instead, use a `case` expression:
f3 x = case x of Baz1 a b -> a==b
```

In general, you can only pattern-match on an existentially-quantified constructor in a `case` expression or in the patterns of a function definition.

The reason for this restriction is really an implementation one. Type-checking binding groups is already a nightmare without existentials complicating the picture.

Also an existential pattern binding at the top level of a module doesn't make sense, because it's not clear how to prevent the existentially-quantified type from "escaping".

So for now, there's a simple-to-state restriction. We'll see how annoying it is.


3. **You cannot use existential quantification for `newtype` declarations**

```hs
-- this is illegal:
newtype T = forall a. Ord a => MkT a
```

The reason is that a value of type `T` must be represented as a pair of a dictionary for `Ord t` and a value of type `t`. That contradicts the idea that `newtype` should have no concrete representation.

You can get just the same efficiency and effect by using `data` instead of `newtype`. If there is no overloading involved, then there is more of a case for allowing an existentially-quantified `newtype`, because the `data` version does carry an implementation cost, but single-field existentially quantified constructors aren't much use. So the simple restriction (no existential stuff on `newtype`) stands, unless there are convincing reasons to change it.


4. **You can't use `deriving` to define instances of a data type with existentially quantified data constructors**.

The reason being that in most cases it would not make sense.

```hs
data T = forall a. MkT [a] deriving( Eq )
```

To derive `Eq` in the standard way we would need to have equality between the single component of two `MkT` constructors:

```hs
instance Eq T where
  (MkT a) == (MkT b) = ???
```

But `a` and `b` have distinct types, and so can't be compared. It's just about possible to imagine examples in which the derived instance would make sense, but it seems altogether simpler simply to prohibit such declarations. Define your own instances!
