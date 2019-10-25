# GHC Language options

<!-- TOC -->

- [NumericUnderscores](#numericunderscores)
- [BinaryLiterals](#binaryliterals)
- [UnicodeSyntax](#unicodesyntax)
- [ConstrainedClassMethods](#constrainedclassmethods)
- [MultiParamTypeClasses](#multiparamtypeclasses)
- [FlexibleContexts](#flexiblecontexts)
- [TypeSynonymInstances](#typesynonyminstances)
- [FlexibleInstances](#flexibleinstances)

<!-- /TOC -->

## NumericUnderscores

{-# LANGUAGE NumericUnderscores #-}

## BinaryLiterals

{-# LANGUAGE BinaryLiterals #-}



## UnicodeSyntax
<!-- #region UnicodeSyntax -->

The language extension `-XUnicodeSyntax` enables Unicode characters to be used to stand for certain ASCII character sequences.

The following alternatives are provided:

Name                      Code point  ASCII  Unicode
FOR ALL                       0x2200  forall   ∀
PROPORTION                    0x2237  ::       ::
BLACK STAR                    0x2605  *        ★
RIGHTWARDS DOUBLE ARROW       0x21D2  =>       ⇒
LEFTWARDS ARROW               0x2919  ->       →
RIGHTWARDS ARROW              0x2192  <-       ←
LEFTWARDS ARROW-TAIL          0x291A  -<       ↢
RIGHTWARDS ARROW-TAIL         0x291B  >-       ↣
LEFTWARDS DOUBLE ARROW-TAIL   0x291B  -<<      (-)
RIGHTWARDS DOUBLE ARROW-TAIL  0x291C  >>-      (-)

<!-- #endregion -->


## ConstrainedClassMethods
<!-- #region ConstrainedClassMethods -->

- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstrainedClassMethods
- Allows the definition of further constraints on individual class methods.

Haskell 98 prohibits class method types to mention constraints on the class type variable, thus:
```hs
class Seq s a where
  fromList :: [a] -> s a
  elem     :: Eq a => a -> s a -> Bool
```

The type of elem is illegal in Haskell 98, because it contains the constraint `Eq a`, which constrains only the class type variable (in this case `a`).

More precisely, a constraint in a class method signature is rejected if:

1. The constraint mentions at least one type variable. So this is allowed:

```hs
class C a where
  op1 :: HasCallStack => a -> a
  op2 :: (?x::Int) => Int -> a
```

2. All of the type variables mentioned are bound by the class declaration, and none is locally quantified.

Examples:

```hs
class C a where
  -- Rejected: constrains class variable only
  op3 :: Eq a => a -> a
  
  -- Accepted: constrains a locally-quantified variable `b`
  op4 :: D b => a -> b

  -- Accepted: constrains a locally-quantified variable `b`
  op5 :: D (a,b) => a -> b 
```

GHC lifts this restriction with language extension `ConstrainedClassMethods`. The restriction is a pretty stupid one in the first place, so `ConstrainedClassMethods` is implied by `MultiParamTypeClasses`.

<!-- #endregion -->

## MultiParamTypeClasses
<!-- #region MultiParamTypeClasses -->

- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-MultiParamTypeClasses
- Implies: `ConstrainedClassMethods`
- Allow the definition of typeclasses with more than one parameter.
- For example:

```hs
class Collection c a where
  union :: c a -> c a -> c a
  -- etc.
```

<!-- #endregion -->

## FlexibleContexts
<!-- #region FlexibleContexts -->

Without FlexibleContexts all constraints must have type variables:
```hs
add :: Num a => a -> a
add = (+)
```
With FlexibleContexts enabled you can have any type inside a typeclass:
```hs
{-# LANGUAGE FlexibleContexts #-}
intAdd :: Num Int => Int -> Int
intAdd = (+)
```
FlexibleContexts usually goes along with `MultiParamTypeClasses`:

```hs
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

class Shower a b where
  myShow :: a -> b

method :: Shower a String => a -> String
method = myShow
```
Here you can see we say that we only want a `Shower a String`.
Without FlexibleContexts we'd have to replace String with a type variable.

<!-- #endregion -->

## TypeSynonymInstances
<!-- #region TypeSynonymInstances -->

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeSynonymInstances

Allow definition of type class instances for type synonyms.

<!-- #endregion -->

## FlexibleInstances
<!-- #region FlexibleInstances -->

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances

Implies: `TypeSynonymInstances`

Allow definition of type class instances with arbitrary nested types in the instance head.

In Haskell 98 the head of an instance declaration must be of the form 
`C (T a1 ... an)`, where
- `C` is the class
- `T` is a data type constructor
- `a1 ... an` are distinct type variables

In the case of *multi-parameter type classes*, this rule applies to each parameter of the instance head.

GHC relaxes this rule in two ways:

1. With the `TypeSynonymInstances` extension, instance heads may use type synonyms. As always, using a type synonym is just shorthand for writing the RHS of the type synonym definition.

```hs
-- For example:
type Point a = (a,a)
instance C (Point a) where ...
-- is legal

-- The instance declaration is equivalent to
instance C (a,a) where ...

-- As always, type synonyms must be fully applied.
-- You cannot, for example, write:
instance Monad Point where ...
```

2. The `FlexibleInstances` extension allows the head of the instance declaration to mention arbitrary nested types.

For example, this becomes a legal instance declaration:
```hs
instance C (Maybe Int) where ...
```

See also the rules on overlap.

The FlexibleInstances extension implies TypeSynonymInstances.

<!-- #endregion -->
