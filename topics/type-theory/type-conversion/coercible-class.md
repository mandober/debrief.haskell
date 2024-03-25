# Coercible class

The `Coercible` class is defined in the `GHC.Types` module

```hs
-- Defined in GHC.Types
type role Coercible nominal representational representational
type Coercible :: forall k. k -> k -> Constraint
class Coercible k a b => Coercible `k a b
```

> The `Coercible` is a two-parameter class that has instances for types `a` and `b` if the compiler can infer that they have the same representation.

> The `Coercible` class doesn't have regular instances - instead they are created on-the-fly during type-checking.

Trying to manually declare an instance of `Coercible` is an error.

Coercible is not a class, but a **special constraint with custom solving rules**.

## Time to pretend

Nevertheless, we can pretend that if it were a class it would be defined as:

```hs
-- Loosely, if Coercible were a class
class Coercible a b where
  coerce :: a -> b
```

Then the following 3 kinds of instances would exist:
1. Reflexive coercions: `instance Coercible a a`
2. Coercions under the same type ctor
3. Newtype coercions: between `T` and its newtype `NT`


1. First, a trivial base-case corresponding to reflexivity:

```hs
instance Coercible a a
```

2. For every type constructor there is an instance that allows coercion under that type constructor.

For example, let `D` be a prototypical type ctor (data or newtype) with 3 type args having the resp. roles: `nominal`, `representational`, `phantom`. Then there is an instance of the form:

```hs
instance Coercible b b' => Coercible (D a b c) (D a b' c')
```

Notice that
- `nominal` type args are equal.
- `representational` tyargs may differ but need `Coercible` instances themself
- `phantom` type args may be changed arbitrarily.


3. The third kind of instance exists for every `T` and `newtype NT = MkNT T`, and comes in two variants:

```hs
instance (Coercible a T) => Coercible a NT

instance (Coercible T b) => Coercible NT b
```

This instance is only usable if the data constructor `MkNT` is in scope.


If, as a library author of a type ctor like `Set a`, you want to prevent users to write `coerce :: Set T -> Set NT`, you need to set the role of Set's type arg to `nominal`, by writing:

```hs
type role Set nominal
```



For more details about this feature, see the paper:

* Safe Coercions - J.Breitner, R. Eisenberg, S. P. Jones, S. Weirich
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf
