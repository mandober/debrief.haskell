# Functors :: Functors in Haskell

## Functor methods

The `Functor` class comes with the main method `fmap`, but there are also some auxillary methods associated with it.
1. `fmap`
2. `<$>`   fmap as operator
3. `<$`    replacing contents with a constant value
4. `$>`    flipped (<$)
5. `<&>`   flipped (<$>)
6. `void`

```hs
fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<&>) :: Functor f => f a -> (a -> b) -> f b

(<$)  :: Functor f => a -> f b -> f a
($>)  :: Functor f => f b -> a -> f a

void  :: Functor f => f a -> f ()
```

### fmap

The `fmap` function is the primary method, the reason d'etre of the `Functor` class.

`fmap` is used to apply a function of `g :: a -> b` to a value of type `f a`, where `f` is a functor, to produce a value of type `f b`. It is also said that the function `g` is *lifted*.

Note that any type ctor with more than one parameter (e.g. `Either`), only the last type param is modified with `fmap` (e.g. `b` in `Either a b`).

Some type ctors with 2 or more params have a `Data.Bifunctor` instance that allows both the last and the penultimate params to be mapped over.

Examples

```hs
-- Convert from Maybe Int to Maybe String using show
>>> fmap show Nothing               -- Nothing
>>> fmap show (Just 3)              -- Just "3"

-- Convert from Either Int Int to Either Int String using show
>>> fmap show (Left 17)             -- Left 17
>>> fmap show (Right 17)            -- Right "17"

-- Double each element of a list:
>>> fmap (*2) [1,2,3]               -- [2,4,6]

-- Apply even to the second element of a pair:
>>> fmap even (2,2)                 -- (2,True)

-- Apply even to the last arg of a heterogeneous tuple:
>>> fmap even ("hello", 1.0, 4)     -- ("hello", 1.0, True)
```

It shouldn't be surprising that `fmap` only maps the last arg, i.e. a polyadic data type is *functorial* in the last type param only. This should explain why `fmap` can be used with heterogeneous tuples, as in the last example above.


### <$>

The `<$>` operator is merely the infix form of `fmap`.

### <$

The `<$` operator is like a deviant `fmap` lacking a whole mapping function `a -> b`, only having a constant value of type `b`. The only thing it can do is replace all the elements of the target container with it.

```hs
(<&>) = flip fmap
(<$>) = fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
-- no mapping, only a "return value"
(<$)  :: Functor f =>       b  -> f a -> f b
-- after renaming
(<$)  :: Functor f =>       a  -> f b -> f a
-- after flipping the params
($>)  :: Functor f => f b -> a  -> f a
($>) = flip (<$)
```

The `<$` operator cannot apply a function, as there is no function `a -> b` to apply - there is only a constant value of type `b`. All it can do (and that's what it is used for) is to replace all the elements in the container `f a` with that constant value `b` to get a container full of `b`'s, `f b`. There is no *lifting* of a function with `<$` (maybe a lifting of a value?).

Identifier defined in 'GHC.Base'.

```hs
(<$) :: forall (f :: Type -> Type) a b.
        Functor f =>
        a -> f b -> f a
```

Replace all locations in the input with the same value. The default definition is `fmap . const`, but this may be overridden with a more efficient version.

Replace all locations in the input with the same value. The default definition is `fmap . const`, but this may be overridden with a more efficient version.

Examples

* Perform a computation with 'Maybe' and replace the result with a constant value if it is 'Just':

>>> 'a' <$ Just 2  -- Just 'a'
>>> 'a' <$ Nothing -- Nothing


### $>

The `$>` operator is just a version of the `<$` with parameters flipped. 
Identifier defined in 'Data.Functor'.

```hs
($>) :: forall (f :: Type -> Type) a b.
        Functor f =>
        f a -> b -> f b
($>) = (<$)
```

Examples

```hs
-- Replace the contents of Maybe Int with a constant String
e1x1 = Just 90210 $> "foo"     -- Just "foo"
e1x2 = Nothing    $> "foo"     -- Nothing
-- but
e1x3 = Nothing   <$  "foo"     -- [Nothing,Nothing,Nothing]


-- Replace the contents of Either Int Int with a constant String,
-- resulting in Either Int String:
x4 = Left 8675309 $> "foo"  -- Left 8675309
x5 = Right 8675309 $> "foo" -- Right "foo"

-- Replace each element of a list with a constant String
x6 = [1,2,3] $> "foo" -- ["foo","foo","foo"]

-- Replace the second element of a pair with a constant String:
x7 = (1,2) $> "foo" -- (1,"foo")
```



### <&>

The `<&>` operator
- defined in Data.Functor
- not avaialble in Prelude
- merely a flipped version of (<$>)

```hs
(<&>) :: forall (f :: Type -> Type) a b. Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
```

Examples
```hs
apply (+1) to a list, a Data.Maybe.Just, and a Data.Either.Right

x1 = Just 2  <&> (+1)  -- Just 3
x2 = [1,2,3] <&> (+1)  -- [2,3,4]
x3 = Right 3 <&> (+1)  -- Right 4

y1 = (+1) <$> Just 2   -- Just 3
y2 = (+1) <$> [1,2,3]  -- [2,3,4]
y3 = (+1) <$> Right 3  -- Right 4
```


### void

The `void` function. Identifier defined in 'Data.Functor'. 
`void value` discards or ignores the result of evaluation, such as the return value of an 'System.IO.IO' action.


```hs
void :: forall (f :: Type -> Type) a. Functor f => f a -> f ()
```

Examples

```hs
-- Replace the contents of a 'Maybe Int' with unit:
>>> void Nothing   -- Nothing
>>> void (Just 3)  -- Just ()

-- Replace the contents of an 'Either Int Int' with unit
-- resulting in an 'Either Int ()'
>>> void (Left 8675309)   -- Left 8675309
>>> void (Right 8675309)  -- Right ()

-- Replace every element of a list with unit:
>>> void [1,2,3] -- [(),(),()]

-- Replace the second element of a pair with unit:
>>> void (1,2) -- (1,())

-- Discard the result of an 'System.IO.IO' action:
>>> mapM print [1,2]
-- 1
-- 2
-- [(),()]
>>> do
  _ <- mx -- vs
  void mx

>>> void $ mapM print [1,2]
-- 1
-- 2
```
