# Church encoding of ADTs

Encoding algebraic data types, particularly, product and sum types, using Church (Church-Scott) encoding.

## Defining ADTs

- constructor functions
  - data constructor
  - smart constructor
- deconstructor functions

Every algebraic data type has two sets of associated functions: constructors and deconstructors. In fact, we could distinguish more phases in the lifecycle of an ADT:
1. definition (of the type)
2. creation (of its values)    via the constructors
3. consumption (of the values) via the deconstructors
4. destruction (of the values) via the destructors

In Haskell, GC is in charge of the value destruction, so we don't particularly discuss that phase; it is listed just to emphizes the fact that a destructor function destroys a value, while a deconstructing function deconstructs it, which is a very different thing.

Constructors are about *synthesis* because they compose the constituent parts (which are usually simpler values) to create a compound value. Deconstructors are about *analysis* since they decompose a compound value into its constituent parts.

in Haskell, the ctor sets of functions is the priviledged one since ctors are directly integrated in the definition of an ADT. In fact, it can be said that an ADT is defined in terms of its constructors, which, due to this role, are called *data (or value) constructors*.

```hs
-- Either is the canonicial sum type
data Either a b = Left a | Right b

-- The types of its data ctors
Left  :: a -> Either a b
Right :: a -> Either a b
```

The definition above shows how the `Either` data type is defined in terms of its two ctor functions.

However, there's no particular reason to be so - it would also be possible that a mechanism for defining a type be separate form the means of creating the values of that type; the same way as the actual manner where consuming a type is separate from defining the type and from creating its values.

After all, data constructors are not the only functions capable of constructing the terms of an ADT - module authors may define any number of such functions, especially when employing the *smart constructor* programming patter.


> What does it mean to apply a data ctor?

Push come to shove, creating a value of an ADT is no more complicated than tagging the supplied value with a particular label.

For example, `Either` data type has 2 ctors that are practically the same intrinsicaly, but conventionally each one has a predefined (albeit arbitrary) purpose - `Left` is used to hold the error message in case of failure, while `Right` holds the payload in case of success. Using these two ctors in exactly this fashion isn't enforced, but only a metter of convention; for instance, someone ignoring these conventions may switch their roles and place the payload in the `Left` ctor.

```hs
Left  :: a -> Either a b
Right :: b -> Either a b

x1 :: Either String b
x1 = Left "Overflow occured"

x2 :: Either a Int
x2 = Right 42

x3 :: Either String Int
x3 = ???

-- 1) using a deconstructing function `either`
dcon1 :: Either String Int -> String
dcon1 x = either (++ "!?") ((++ "?!") . show) x

r1 = dcon1 x1 -- "Overflow occured!?"
r2 = dcon1 x2 -- "42?!"


-- 2) using pattern matching
dcon2 :: Either String Int -> String
dcon2 x = case x of
    Left  s -> s ++ "!?"
    Right n -> (++ "?!") . show $ n

r3 = dcon2 x1 -- "Overflow occured!?"
r4 = dcon2 x2 -- "42?!"
```




On the one hand, In Haskell, an ADT is defined in terms of its constructor functions, called data constructors, which are used to create values of that type. On the other hand, to consume a value of such type, we use deconstructor functions that analyze the value into its constituent values.

The correspondences between Haskell's ADTs and inference rules in logic

Haskell           | Inference
------------------|--------------------
type definition   | formation rule
value creation    | introduction rule
value consumption | elimination rule

In Haskell, an ADT type definition corresponds to a formation inference rule in logic.

In Haskell, to consume an ADT, it must be first destructured via the mechanism of pattern matching, where it is matched against a pattern that can distinguish the actual data ctor used to create the value (in case of sum types). A value of the sum type is said to be tagged by one of the data ctors used to define that type.

Whether pattern matching is used, but especially if a standalone destructuring function is used, scrutinizing a value means that we apply the dtor function (e.g. `either`) to the appropriate value.

The value itself is defined in terms of the 2 data ctors, and the dtor function (`either`) expects that we supply it with the handlers for both cases, i.e. for both data ctors. it expects the first arg to be the function to handle the `Left` case, the second arg to be the function to handle the `Right` case, and the thirst arg is the value itself, of the `Either` type.

When `either` is applied to the appropriate value (e.g. `x :: Either a b`), the approapriate handler function is selected. The value itself  (being defined in terms of the two data ctors) selects the appropriate handler (it can't help it), thereby extracting the payload of the selected variant.


```hs
data Either a b = Left a | Right b

-- 2 ctor functions
Left  :: a -> Either a b
Right :: a -> Either a b

-- 1 standard dtor function
either :: (a -> r) -> (b -> r) -> Either a b -> r
```



---

For example, the canonical coproduct type, `Either`, is defined in terms of its 2 ctors, `Left` and `Right`:

Encoding an ADT, especially a sum type in terms of ctor and dtor hofs
- The function that represents the datatype works as an dtor.

We first consider Haskell's canonical sum type, Either a b, that is a coproduct whose values are created using one of the 2 available ctor functions. In fact, both of these are data ctors (Left and Right) that are integrated in the type declaration itself. To destructure a value of `Either a b` type, there is 1 dtor function, `either`.

```hs
-- coproduct
data Either a b = Left a | Right b
-- 2 ctors
Left  :: a -> Either a b
Right :: a -> Either a b
-- 2 dtor
either :: (a -> r) -> (b -> r) -> Either a b -> r

-- product
data Pair a b = (a,b)
-- 1 ctor
Pair a b = (a,b)
-- 2 dtors
fst :: Pair a b -> a
snd :: Pair a b -> b
```

Generally, a canonical coproduct type (Either a b) value will have 2 ctors and 1 dtor. But a canonical product type, (a, b), value will have 1 ctors and 2 dtors.

However, despite the symmetry of a product and coproduct, there is an evident disbalance concerning the type `r` that only appears in the coproduct's dtor function.


> Could it correspond to something in the Pair data type as well, in order to restore the symmetry?


The `either` dtor is very similar to the ∨E rule from logic:

          [a]      [b]
           ⁝         ⁝
           r        r
        -------  -------
a ⋁ b   a -> r   b -> r
------------------------
            r

If you have a proposition (a ⋁ b), assuming a and b, separately, allows us to conculude (a -> r) and (b -> r), respectively, which then allows us to finally derive `r`. In both cases, in logic as well as in Haskell, we perform the case analysis on the value or on the proposition (a ⋁ b).

In Haskell, the case analysis is performed by pattern matching against a value of `Either a b` type, which reveals the data ctor it contains (Left or Right).

In case `Left` is found, we treat the payload in one way, and in case `Right` is found we treat the payload in another way.

The main value we're intereseted in, the payload, is in fact two separate values, 2 payloads, that potentially have two distinct types. The payload value in Left will have the type `a`, and the one in Right will have the type `b`.

These 2, potentially, different types are the reason that we can't just return either payload directly when we pattern match it. It is disallowed to have incompatible branches, type-wise.

Therefore, we supply 2 handler functions, one for each case, that will be applied to their respective payloads, producing the value of the same type `r`,
which can then be returned. It is very similar in both Haskell and logic.



In fact, a datatype is repr by the dtor function(s).

a value of the Either type is encoded in such as way that it holds both dtors; then instead of pattern matching against such a value, we instead pass the value itself to the proper dtor function.

The value is encoded in such a way, 
that when the dtor is applied to it, 
the appropriate handler function 
(hLeft or hRight) is selected, 
thereby extracting the payload.


Upon constructing a value of this type, apart from the payload, we also need to provide both handler functions.
