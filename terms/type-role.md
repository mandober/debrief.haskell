# Type role

https://www.youtube.com/watch?v=0udX2HqFUD8

A type prescribes the way in which the data is to be treated. A type may be considered an interface through which we interact with the underlying data.

Sometimes we want to change that interface sligthly, without touching the underlaying implementation. One way to do this in Haskell is using the `newtype` mechanism which allows us to make a copy of a type, to which we can then apply appropriate restrictions; that is, we apply the constraints on functions that operate on that type.


The newtype mechanism is applicable only to the types that have a single constructor.

We can extend a type τ by making a newtype σ based on it. The types thusly obtained are isomorphic to each other: τ ≅ σ

Since the types τ and σ have the same underlying representation, it is desirable if we can reuse the functions that are already available on the original type τ for the newtype σ.

* newtype - redesign the interface of a type while maintaining the implementation
* coerce - coerce between the newtype and original type at no run time cost

```hs
-- newtype
newtype Age = MkAge Int

-- coerce
ageNextYear :: Age -> Age
ageNextYear = coerce (+1)
```

The `coerce` function tells the typechecker to change the type from `Int -> Int` to `Age -> Age` type at compile-time, but coerce disappears at runtime; it has no RT cost.


Haskell has *type-directed computations* with *type families* which can distinguish between a newtype and the original type.

```hs
type family   AgeToString (x :: *) :: *
type instance AgeToString Age = String
type instance AgeToString Int = Bool
```

Having both coerce and type family creates a problem:

```hs
wrong :: String
wrong = coerce True
```

If Int and Age are repr the same then maybe `AgeToString Age` and `AgeToString Int` are also the same and we can coerce between the two? However, they are not and coercing between them is wrong - we cannot coerce `True` to `String`.

Bool = AgeToString Int ==?== AgeToString Age = String

So, although Age and Int appear to be the same, they are not completely the same. There are two views i.e. two notions of equality, CT and RT equality.

- Nominal type equlity = equality at compile time
- Representational type equlity = equality at run time
- Nominal ⊂ Representational
