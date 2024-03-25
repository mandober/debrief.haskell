# Reader

## Intro

## Function type

The function type in its most generic form is `(->) a b` aka `a -> b`. This is more of a "template type" than a concrete type since there is no function as general as `forall a b. a -> b`, but the concrete signature of any function conforms to this, general, signature.

This is the same level of generality as the general *product* type `(,) a b`, or a general *coproduct* type `Either a b`, except while we really can plug in any type in place of `a` or `b` in both product and coproduct, we cannot do so in function type. ~~Is this even true? It seems very doubtful. Why is this even mentioned; it has nothing to do with the topic. You just can't curb it down, can you?~~

We can imagine that the function type could have been defined similarly to:

```hs
data (->) a b = (->) a b
-- or equivalently as
data Function a b = Fn a b
```

The important thing here is the position of type variables `a` and `b`. That is, their position of the left side, that defines the type, and their position of the right side, that specifies the form the values of this type take. Namely, the right side shows the use form, which is `Fn a b`, meaning that type param `a` is the input type, while type param `b` the output (return) type. The left side lists these two params exactly in that order, `Function a b`. This is important for partial type application. The form `Function a` signifies the partially applied type ctor `Function`. We say that the `Function` type ctor has the first type param (`a`) fixed. Since `a` is the input type this form (partially applied type ctor, `Function a`) is the type suitable to receive the `Functor` instance. This is because the type `Function a b` represents functions in general, and functions are very peculiar. Namely, function type is the only type that exhibits both kinds of *variance*:
>it is contravariant in the input type, but covariant in the output type.

Functions are functors in the return type. Thus, the type ctor with the input type (`a`) fixed, `Function a`, is a functor in the return type (`b`). The input type is fixed while the return type varies. More precisely, this form is an instance of the *covariant functor type class*, which is denoted and called `Functor`. Another type class, `Contravariant` represents the contravariant functors.

As already mentioned, `Function a` is a suitable receiver for the `Functor` class. We can easily express partial application of a type ctor to its first type param, but not to its others type params individually (we canot skip type param). And since the function type is defined the way it is - with the type params on the left side declared in the same order they are used on the right side - we get the appropriate form, `Function a`, for the `Functor` class.

However, there is no way to express the partial application of the `Function` type ctor to its second type param (`b`). That is, we cannot fix the return type while letting the input type vary. At least not with function type expressed like it is. But we can express it differently. We can make a type wrapper, called `Op`, with the type params order reversed.

```hs
data Operation a b = Op b a
-- or
data Operation b a = Op a b
```

We can reverse their order on the left xor on the right side. The important thing is that the partial application form `Operation x` represents the function type with the *return type fixed* (here it is `Operation b` since `b` represents the return type). Now we can make this type an instance of the `Contravariant` class.

However, the function type is weirder still. We can combine the two possible maps (co- and contravariant ones) into a single type - the `Profunctor` type class. Profunctors have two maps, of which one map (contravariant) goes in the opposite direction (compared to a `Bifunctor`).

```hs
instance Functor f where
  fmap :: (a -> b) -> (f a -> f b)

instance Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)

class Bifunctor f where
  bimap :: (a -> b) -> (c -> d) -> (f a c -> f b d)

class Profunctor f where
  dimap :: (b -> a) -> (c -> d) -> (f a c -> f b d)
```



## Reader data type

The `Reader` data type just wraps the general function type `a -> b`. 

```hs
newtype Reader r a = Reader { runReader :: r -> a }
```

The first type param `r` is the input type to the function type `r -> a`, and the second type param `a` is the ouput, so it is exactly the function type.

However, the `Reader` moand is more associated to having a global read-oply environment. That is, we use the `Reader` to model a global environment, like a configuration that needs to be accessible to functions that require it. We can do this using the `State` monad, but since the environment is read-only it would be an overkill. Since functions only need to reference the glabal environment, and not change it, they also need not return it. This should simplify the implementation.

Just like we did with `State`, we can also add an extra param that carries the environment to functions. Let's see how it fairs when we manually thread the environment through functions. We want to capture the emerging pattern into an abstraction to lessen the expected boilerplate. The functions need not make changes to the environment, so they probably neeed not return the environment.

## Manually theading the environment

A global environment is often use to hold the app-wide configuration, perhaps with the info of how to connect to a db in out current dev env. The exact details of what is inside the config are not important, so let's say we are doing some very basic tests, and we have a boolean flag that signifies whether a test is enabled or not.

The config is defined as a data type `ABConfig`, which allows to set whether the letters 'E' and 'L' should be processed or skipped (for whatever reason). We also create an instance of the config `cfg`, which says that the two letters should be skipped, and we'll pass the `cfg` to all functions as an environment.

```hs
-- global config is just a data type
data ABConfig = ABConfig
  { noLetterE :: Bool
  , noLetterL :: Bool
  }

-- instance of the config
cfg :: ABConfig
cfg = ABConfig
  { noLetterE = True
  , noLetterL = True
  }
```

The function `toUpperStr` uppercases a given string, but it needs to consult the config to see whether the letters 'E' and 'L' need to be processed or skipped.

```hs
-- Uppercase the string, obeying current tests.
toUpperStr :: ABConfig -> String -> String
toUpperStr env str = filterBy filters upper
  where
  upper = fmap toUpper str
  filters = [ if noLetterE env then (/= 'E') else const True
            , if noLetterL env then (/= 'L') else const True
            ]
  filterBy [] ys     = ys
  filterBy (x:xs) ys = filterBy xs (filter x ys)

x1 :: String
x1 = toUpperStr cfg "elon" -- "ON"
```

If there was no `ABConfig` parameter, the function type would be just `String -> String`, the same as the `toUpper` function.

We can then use the `toUpperStr` function in other definitions:

```hs
welcomeMessage :: ABConfig -> String -> String -> String
welcomeMessage cfg motd username = "Welcome, "
  ++ toUpperStr cfg username
  ++ "! Message of the day: "
  ++ toUpperStr cfg motd

x2 :: String
x2 = welcomeMessage cfg "foo bar" "Elon"
-- "Welcome, ON! Message of the day: FOO BAR"
```

The function `fullName` takes in a first name, last name, and nickname, and outputs the full name, with the nickname in-between the other two names, in quotes. We use `toUpperStr` to transform all three name components.

```hs
fullName :: ABConfig -> String -> String -> String -> String
fullName cfg fn nn ln = toUpperStr cfg fn
  ++ " \""
  ++ toUpperStr cfg nn
  ++ " \""
  ++ toUpperStr cfg ln

x3 :: String
x3 = fullName cfg "Cinder" "Chonk Boy" "Block"
-- "CINDR \"CHONK BOY \"BOCK"
```

## Extracting the core type

This works, but we'd like it to be more comfortable. It is not ideal that we must manually pass the config around since there is still the possibility of accidentally mixing up parameter order, if the type of the config coincides with something else a function needs. We'd like to automate the boilerplate.

We assume a monad will halp us achive that automation, but we need to come up with a data type to make it the carrier of various classes.

Looking at the signatures of the functions so far, we need to extract the "core type" that represents the essence of a global environment.

```hs
toUpperStr     :: ABConfig -> String -> String
welcomeMessage :: ABConfig -> String -> String -> String
fullName       :: ABConfig -> String -> String -> String -> String
```

The `String` parameters don't seem like they have much to do with our problem, since they are specific to each function in question. Neither does the `String` return type, so we can abstract them. Clearly we need some type for the config we pass in, but the type of config may also be abstracted. In the end, the type we are left with is:

```hs
env :: e -> a
type Env e a = e -> a
```

which is just a function type `a -> b`. For now, we'll roll with this type: the core of a configuration function is, tautologically, a function that takes in a config, `e`, and produces a value, `a`.

Augmented with an accessor function, this type in std lib appears as

```hs
data Reader r a = Reader { runReader :: r -> a }
```

## ReaderT monad transformer

Note: truth be told, the `Reader` type is actually defined as a type alias in terms of the reader monad transformer, `ReaderT`, using the identity functor in place of a monad. Monad transformers are data types that allow us to roll two monads into one that incorporates the behaviors of both.

### Quiz: ReaderT definition

Taking into account the fact that a monad transformer combines the behaviors of two monads into one producing a monad that incorporates the behaviors of both: 
>What is the correct placement for the "other" monad `m` on the right-hand side of the equation for `ReaderT`? Why?

```hs
newtype ReaderT m r a = ReaderT    (m r ->   a)  -- (A)
newtype ReaderT m r a = ReaderT    (  r -> m a)  -- (B)
newtype ReaderT m r a = ReaderT    (m r -> m a)  -- (C)
newtype ReaderT m r a = ReaderT (m (m r -> m a)) -- (D)
newtype ReaderT m r a = ReaderT (m (  r -> m a)) -- (E)
newtype ReaderT m r a = ReaderT (m (m r ->   a)) -- (F)
newtype ReaderT m r a = ReaderT (m (  r ->   a)) -- (G)
```

Spoiler ahead!

## How does monads help us

We have come up with the dta type

```hs
newtype Reader r a = Reader { runReader :: r -> a }
```

but, technically, we don't know whether this will form a valid monad just by looking at it. All we can do is give it a shot, try to write the instances, and see if what we end up with obeys the monad laws.

>But before we dive into implementing, why would doing this transformation and rewriting all our functions to use this datatype even help us in the first place?

Recall the functions that a monad gives you: `return` and `>>=` with types:

```hs
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
```

>How do the types of these functions relate to our actual datatype?

The `m` type param in there will be the type ctor `Reader r`.

```hs
newtype Reader r a = Reader (r -> a)

return :: a -> Reader r a
(>>=)  :: Reader r a -> (a -> Reader r b) -> Reader r b
```

The **config has been abstracted** underneath the `m` type parameter in (>>=); as we wanted, the only thing we need to worry about - and the only thing we have access to - is the original return value (of the function).

If we construct our functions using (>>=), the boilerplate of passing the config parameter to each function disappears.

After we implement the FAM instances to do the config passing for us, we might end up with something like:

```hs
toUpperStr :: String -> Reader ABConfig String
toUpperStr str = Reader (\cfg ->
  let filters :: [Char -> Bool]
      filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                , if don'tUseLetterL cfg then (/= 'L') else const True
                ]
      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  in filter passesFilters (fmap toUpper str))

welcomeMessage :: String -> String -> Reader ABConfig String
welcomeMessage motd username =
  toUpperStr motd >>= (\upperMOTD ->
    toUpperStr username >>= (\upperUsername ->
      Reader (\_ -> "Welcome, "
        ++ upperUsername
        ++ "! Message of the day: "
        ++ upperMOTD)))

fullName :: String -> String -> String -> Reader ABConfig String
fullName firstname nickname lastname =
  toUpperStr firstname >>= (\upperFname ->
    toUpperStr nickname >>= (\upperNick ->
      toUpperStr lastname >>= (\upperLname ->
        Reader (\_ ->
          upperFname ++ " \"" ++ upperNick ++ " \"" ++ upperLname))))
```

Nnotice that in `welcomeMessage` and `fullName`, all mention of the config parameter inside the function body is gone. *`>>=` is now the one responsible for ensuring that all functions are using the same config value*. So whatever implementation we write for the monadic bind, that's where we'll move the parameter passing that was previously done manually.

## FAM instances

Keep in mind that the point of (>>=) and (<*>) is to do the exact same parameter passing that we previously did manually.

```hs
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader k <*> Reader f = Reader \env -> k env (f env)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- (1)
  Reader f >>= k = Reader \env -> runReader (k (f env)) env
  -- (2)
  Reader f >>= k = Reader \env ->
    let a  = f env
        rb = k a
    in  runReader rb env
```

There are shorter ways to write these instances using function combinators, but these are probably the most easily-understood definitions.

The most important thing to notice is in the definitions of (<*>) and (>>=).
>See how we are passing the `env` parameter twice, to both of the functions contained within the `Reader` values?
That is the secret of how the same configuration gets threaded through our whole stack of functions.

## Primitives

Since we are using monadic binds, we have abstracted over our config parameter and don't have a convenient way to access it. This is fine for functions that need not refer to it, but a problem for functions like `toUpperStr` that do.

We could directly use the `Reader` constructor to write these functions, but having to break open the internals of our abstraction just to write something so simple seems wrong. Instead, we should come up with a few primitives that operate on the `Reader` to provide that functionality to the instance monad.

```hs
ask :: Reader r r
ask = Reader (\env -> env)
-- or
ask = Reader id


asks :: (r -> a) -> Reader r a
asks f = Reader (\ env -> f env)
-- i.e.
asks f = Reader f
-- i.e.
asks   = Reader
-- or, in terms of `ask`
asks f = do
  env <- ask
  return (f env)

local :: (r -> r)   -- The function to modify the environment
      -> Reader r a -- Computation to run in the modified environment.
      -> Reader r a -- Execute a computation in a modified environment

```

Remember that for `ask`, we want the functions we write to be able to read the current config, but the only part of the datatype, the functions can interact with, is the return value `a`; so, we need to wrap the function `r -> r` in the Reader data ctor to get the type `Reader r r`. It repeats and thus exposes the hidden config as the return type.

It is also common to apply a function that transforms the config locally, or otherwise projects specific fields. The function `asks` is such a convenience function - it applies the given function to the config and returns the result. It doesn't expose the hidden config, though, it only transforms it in the "background". Actually, that transformation is a projection, so it does expose, but only a specific piece (field) of the config.



## Ref

https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html
