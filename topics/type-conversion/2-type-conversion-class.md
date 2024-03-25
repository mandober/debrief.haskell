


## Isomorphism in CT

Two types `a` and `b` are isomorphic if there is a function `f : a -> b` and its inverse function `g` equal to `f⁻¹ : b -> a`, such that
- `f⁻¹ . f = 1ᵇ`
- `f . f⁻¹ = 1ᵃ`

```hs
   a --------f-------> b
1ᵃ ↻ <-------g-------- ↺ 1ᵇ

-- there is a function
f : a -> b
-- and its inverse, function g
g = f⁻¹ : b -> a
-- so
(f . f⁻¹) b = f (f⁻¹ b) = f a = b
(f⁻¹ . f) a = f⁻¹ (f a) = f⁻¹ b = a
-- such that
1ᵇ . f   = f   = f   . 1ᵃ
1ᵃ . f⁻¹ = f⁻¹ = f⁻¹ . 1ᵇ

1ᵇ ∘ f = f = f ∘ 1ᵃ
1ᵃ ∘ g = g = g ∘ 1ᵇ

f ∘ g = 1ᵇ
g ∘ f = 1ᵃ
```

When two objects `a` and `b` are isomorphic to each other, that means that from the perspective of all other objects in a category (as related by the arrows) they are indistiguishable - *they are the same (up to an isomorphism)*; the isomorphism in "up to an isomorphism" are the two morphisms, `f` and `g` between them, which make the two objects isomorphic.

The two objects `a` and `b` are isomorphic, denoted by `a ≅ b`.

## Isomorphism in Haskell

The category `Hask` is the category of Haskell types and functions.

Being isomorphic means the two types are *interconvertable*; we can convert from one type into the other and vice versa.

In Hask, this implies that two types `a` and `b` are isomorphic, `a ≅ b`, and thus interconvertable, if there is a pair of mutually invertable functions
- `from :: a -> b`      (*from* `a` into `b`)
- `into :: b -> a`      (from `b` *into* `a`)
- from = into⁻¹
- into = from⁻¹

such that `from` and `into` are each other's inverses
- `f⁻¹ . f = 1ᵇ = id`
- `f . f⁻¹ = 1ᵃ = id`
- so, `from . into = id = into . from`

In fact, while in Haskell the `id` function is the same one, polymorphic function, in category theory, these are two distinct identity morphism: `f ∘ g = 1ᵇ` and `g ∘ f = 1ᵃ`.

```hs
from :: a -> b
into :: b -> a

from . into = id
into . from = id
```

## Isomorphic class

The class `Isomorphic` gathers all pairs of types which support bidirectional conversion.

**Bidirectionality** between two types is ensured through the recursive superclass constraint, which is the same class `Isomorphic`, but with the type parameters flipped. This requires enabling the `UndecidableSuperClasses` pragma.

```hs
{-# LANGUAGE UndecidableSuperClasses #-}

class (From b a) => Into a b where
  into :: a -> b

class (Into b a) => From a b where
  from :: a -> b
```

For example, the types `String` and `Text` are isomorphic (up to some isomorphism).

```hs
instance From String Text where
  from :: String -> Text
  from = T.pack

instance Into Text String where
  into :: Text -> String
  into = T.unpack
```

Fact is, two types are almost never completely identical. The acceptable level of tolerence needed to consider them "practically identical" is exactly what the isomorphism represents.

The classes `From` and `Into` are thus impractical as a wholesome solution to the problem of type conversion. Sometimes the difference between two types is in the re/wrapping of type ctors - which is the case with a type `a` and its wrapping as `newtype a` - and sometimes the difference is in the bits of storage a type uses - e.g. `Int` vs `Integer`. It all depends on the acceptable level of difference, and one class (or a pair of classes) are not gonna cut it in all these cases. The utility of the `From` and `Into` is thus questionable (but the knowledge gained is not).

## Laws

Type conversion is based on two types being isomorphic to each other, `a ≅ b`, which holds iff

1. There is a function `into` that takes a `b` to `a`.

```hs
into :: b -> a
into b = a
```

2. There is a function `from` that takes an `a` to `b` 
  (`from` can be defined in terms of `into`).

```hs
from :: a -> b
from = into @a
```

3. For all values of type `b`, converting `b` to `a` with `into`, and then converting `a` back to `b` with `from`, must produce the original value `b`.

```hs
from . into = id
into @a . into @b = id
```

4. For all values of type `a`, converting `a` to `b` with `from`, and then converting `b` back to `a` with `into`, must produce the original value `a`.

```hs
into . from = id
into @b . into @a = id
```


## Bidirectional conversion between types

Bidirectionality is ensured via the **recursive superclass dependency** with the type parameters flipped.

```hs
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

class (From b a) => Into a b where
  into :: a -> b

class (Into b a) => From a b where
  from :: a -> b
```

This definition requires enabling the pragma `UndecidableSuperClasses`. Without it, we'd get this error:

```
Superclass cycle for 'Into'
    one of whose superclasses is 'From'
    one of whose superclasses is 'Into'
• In the class declaration for 'Into'
• Perhaps you intended to use UndecidableSuperClassestypecheck
```

Now, when we define an instance for converting a type `a` into `b`, the GHC insists we also define an instance for converting type `b` into `a`.

These two classes also require enabling the `MultiParamTypeClasses` pragma, which allows mentioning two types (type ctors) in a class head. With that, we are actually relating two types, instead of merely defining behaviors of a single type. Such relational modeling is closer to the spirit of programming in Prolog, and to CT where objects are preferrably defined in terms of their relations to other objects.

## Convert class

Acrually, we don't need two classes, `From` and `Into`, when we can express both as a single class `Convert`. After all, `From` and `Into` are each other's inverses - we can express one in terms of the other by flipping the type args.

```
Convert a b
convert :: a -> b     = from :: a -> b  =  into :: b -> a
convert :: b -> a     = from :: b -> a  =  into :: a -> b
```

`Convert a b` encodes the conversion of type `a` into a type `b`. It is the same as `From a b` (from `a` into `b`) or `Into b a` (into `b` from `a`).


```hs
-- | Convert 'a' into 'b'. From 'a' into 'b'. Into 'b' from 'a'.
class (Convert b a) => Convert a b where
  convert :: a -> b


-- String <=> Text

instance Convert String Text where
  convert :: String -> Text
  convert = T.pack

instance Convert Text String where
  convert :: Text -> String
  convert = T.unpack
```
