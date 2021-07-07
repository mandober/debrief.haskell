# Showing types

The `Show` class takes a saturated type ctor, denoted by class type variable `a`, which means it can be any type whatsoever, as long as it is fully applied (saturated). Actually, for compound types like `Node t` the method signatures should be understood to include the constraint `Show t`.

```hs
type Show :: * -> Constraint
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}

-- show method's general standalone sig
show :: (Show a) => a -> String

-- show method's standalone sig semi-specialized to a concrete type, a ~ [a]
show @[_] :: (Show w) => [w] -> String
show @[a] :: (Show a) => [a] -> String

-- we know that Int is showable so no need for constraint
show @[Int] ::           [Int] -> String
```

**TIP**   
Using *TypeApplications* language extension, you can apply types to the corresponding type variables, similar to System F in which it is necessary to supply the type (type-level) arguments first, followed by regular (term-level) arguments.
* Like term arguments, type arguments must also strictly respect the declaration order - the order in which the type params were stated in the signature. However, there is a subtlety here you have to be wary of: the order depends on constraints and forall declarations (make the signatures explicit)!
* If the GHCi surrounds a type param in braces in the displayed signature, this means that particular type param cannot be supplied explicitly. Devs can also use braces around their type params the same way to forbid type application for various reasons.
* To force GHCi to display a signature for a compound type like `Maybe a`, i.e. for a type with the type ctor specified, `Maybe` but leaving the inner type, `a`, generic, querying for `:t f @Maybe a` won't work. Instead of coming up with a disatisfying instantiation for the type param `a`, you can use the underscore! Actually, in this context, the underscore means "skip type application for this particular type param", and it allows you to leave some type params unspecified while instantiating those that copme later on in the declaration.

```hs ghci
:set -XTypeApplications

:t show
-- > show :: (Show a) => a -> String

:t show @[a]
-- > Not in scope: type variable 'a'

:t show @[Int]   -- instead of this
-- > show @[Int] :: [Int] -> String

:t show @[_]     -- do the underscore
-- > show @[_] :: (Show w) => [w] -> String
```


Skipping type applications

```hs ghci
:t fmap @_
fmap
  :: forall {f :: * -> *} {a} {b}
   . Functor f
  => (a -> b)
  -> f a
  -> f b

:t fmap @_
fmap @_
  :: forall {w :: * -> *} {a} {b}
   . Functor w
   => (a -> b)
   -> w a
   -> w b

:t fmap @_ @_
fmap @_ @_
  :: forall {w1 :: * -> *} {w2} {b}
   . Functor w1
   => (w2 -> b)
   -> w1 w2
   -> w1 b

:t fmap @_ @_ @_
fmap @_ @_ @_
  :: forall {w1 :: * -> *} {w2} {w3}
   . Functor w1
   => (w2 -> w3)
   -> w1 w2
   -> w1 w3

:t fmap @_ @_ @_ @_
-- Cannot apply expression of type '(w0 -> w2) -> w1 w0 -> w1 w2'
--    to a visible type argument '_'
--    • In the expression: fmap @_ @_ @_ @_

:t fmap @[] @[_] @[_]
 :: forall {w1} {w2}
  . ([w1] -> [w2])
  -> [[w1]]
  -> [[w2]]
```

Unfortunately you cannot force particular type param names, it's GHCi's choice.
