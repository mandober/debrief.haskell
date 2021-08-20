# Monomorphic and Polymorphic types

Monomorphic types are concrete, fully saturated, instantiated, final, fully resolved, fully applied types.

```hs
-- fully defined value (a function) of a fully defined type
intId :: Int -> Int
intId x = x
-- fully defined value (a function) of a fully defined type
doubleId :: Double -> Double
doubleId x = x

-- parametrically polymorphic
id :: a -> a
id x = x
-- actually
id :: forall a. a -> a
id x = x
```

The definition of functions stays the same. This kind of polymorphism is called **parametric polymorphism** (*generics*, in other languages). Haskell only allows this if there is indeed a single definition - in other words, you cannot choose the definition of a value based on its type (i.e. without `GADTs`).

It also adds safety through a property called **parametricity**. If we pretend that there are no infinite loops or exceptions (reasoning wo bottoms), then the function is actually fully determined by its type; if we see the type `a -> a`, we know that the corresponding value must be the identity function.

Parametricity decreases the semantic scope (of a function's implementation, i.e. the number of things the function could possibly do) by increasing the number of types a type parameter can be instantiated with. That is, *the bigger the number of applicable types, the less the behaviour they have in common*.

```hs
-- one type -> many behaviours
f :: Int -> Int

-- more types -> less behaviours
g :: Num a => a -> a

-- all types -> one behaviour
h :: a -> a
```

A function `Int -> Int` might do just about anything concerning integers (double it, triple it, multiply it with a constant, return the number of its digits, etc.); it may perform any conceivable (arithmetic) operation, maybe not even involving the supplied integer. There's no way to reason about it, to know what it does. However, by turning a monomorphic type into a type parameter and, possibly, fine-tuning it with class constraints, the number of applicable types is increased. As the number of concrete types that could be instantiated goes up, the behaviour they all share goes down. So, a function of type `a -> a`, where `a` can be any type at all, doesn't have a whole lotta (implementation) options. Come to think about it, it cannot do anything else but return the value it received. That value is polymorphic and it's not to be touched, it cannot be touched.

```hs
-- one type -> all behaviours
m :: (a ~ Int) => a -> a

-- few types -> most behaviours
g :: Integral a => a -> a

-- some types -> some behaviours
m :: Num a => a -> a

-- more types -> less behaviours
t :: Show a => a -> a

-- all types -> one behaviour
z :: a -> a
```


## Case studies

## Identity

A function with the sig like `id` can only be the `id` function.

```hs
id :: a -> a
id a = a
```

BTW, a conditional like `if-then-else` requires a Boolean and the expr (of the same type) for each of the two branches. However, if you just need the `if-then` logically, maybe you can pass `id` for the else branch (as NOP).

```hs
needsParen :: Bool -> Doc -> Doc
needsParen True  = parens
needsParen False = id
```


## Helper function required

A recursive function that mentions a list in its return type is gonna need to start with the empty list, fillingit and passing it recursively, which means you're gonna need a helper function that has one param more then the original function which is used to accept the list. Initially, the original function will pass the empty list to the helper function explicitly; after that, the helper will pass the list to itself.

```hs
fv :: Exp a -> [a]
fv exp = aux exp [] 
    where
        aux (A a)   xs = a : xs
        aux (B a b) xs = aux a xs ++ aux b xs
        -- ...
        aux _ xs = xs
```


However, depending on the data structure, this is not always be the case - you can get away without the helper in some cases of recursive function.

```hs
import Data.List (union)

data LExp v = LVar v | LAbs v (LExp v) | LApp (LExp v) (LExp v)

fv :: (Eq v) => LExp v -> [v]
fv (LVar v)   = [v]
fv (LAbs h b) = [h]  `union` fv b
fv (LApp m n) = fv m `union` fv n
```
