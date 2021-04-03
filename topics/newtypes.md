# The newtype wrapper

- `newtype` can be used only on a single-fielded single-data-ctor types.

- newtype is a no-cost type wrapper that preserves the distinction between the wrapped and the wrapping at CT, after which the wrapping is discarded.

- `type` can only make a type alias for a long-winded type signature; the distinction between the two goes unnoticed by the compiler. what is good for? absolutely nothing. say it again. as a shorthand for devious signatures. You can hide a complicated signature behind an easy-on-the-eyes alias, proceeding to pretend you've understood it and thus it's ok that you've "abstract" it.

- since newtype can have one field, the field accessor is allowed, and, besides pattern matching, it can also be used for unwrapping. **But is it the same?** Does it amounts to the same, with regards to executing effects, whether the inner type was unwrapped with pattern matching on the LHS of a function, or unwrapped on the RHS using the accessor function (usually named `runWrapper`)?

- the wrapped type is isomorphic to its wrapping until the runtime, at which time they are identical. The automatic `Coerce` internal class has the `coerce` method to explicitly convert between the two types to and fro.

- Wrapping a type in a newtype wrapper means going through the unwrap-manipulate- rewrap dance every time you need to work with the inner value.

Reasons:
- to emphasise distinction between types (e.g. Km vs Kg)
- to make a type an instance of different classes


```hs
-- parameterized type wrapper
type InnerType m f a = ...
newtype Wrap m f a = Wrapper { runWrapper :: InnerType m f a }

-- many wrapping styles!
newtype WInt = WInt Int
newtype WInt = WInt (Int)
newtype WInt = WInt { unwrap :: Int }

-- the wrapper type and the wrapped type are isomorphic: a ≅ Wrapped a
newtype Wrapped a = Wrapped a


-- record syntax is allowed as long as there's only a single field
newtype State s a = State { runState :: s -> (s, a) }
-- or delete braces and the accessor fn, putting parens as delimiters
newtype State s a = State (s -> (s, a))

-- | not ok - delete accessors (aka field names)
-- newtype Pair a b = Pair { pairFst :: a, pairSnd :: b }
-- | ok
newtype Pair a b = Pair (a, b)
```
