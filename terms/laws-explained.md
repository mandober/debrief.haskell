# Laws: All

Everything's a **value**. A value may be a scalar value or a function value, but we usually don't know which, and besides these laws must work in either case. In cases we know that a value is more likely a scalar we use x, y, z, and when we shoot for a fn we use f, g, h, but be warned! Most of the time, people don't give a fuck denoting variables properly even if it's obvious that something's just gotta be a scalar (e.g. the last applicative value in A.4 must be lifted scalar; that is if you want to eventially end the computation), so I tend to mend.

$$\not\to$$


## Functors

fmap id   = id                                        F.1 IDENTITY
fmap id ϑ = id ϑ

fmap g . fmap f   = fmap (g . f)                      F.2 COMPOSITION
fmap g . fmap f ϑ = fmap (g . f) ϑ



## Applicative

pure id  <*> x = x                                   A.1 IDENTITY
pure  f  <*> pure x = pure (f x)                     A.2 HOMOMORPHISM
      f  <*> pure x = pure ($ x) <*> f               A.3 INTERCHANGE
pure (.) <*> f <*> g <*> x = f <*> (g <*> x)         A.4 COMPOSITION

`pure` is the function that lifts a value (normal value/function) into the context that applicative is working in.

`<*>` ("splat" operator) is the applicative operation.

(A.1) Much like F.1 this states that `pure id` doesn't touch function/ structure in any way. Same as F.1., after all, it's called Appl.Functor
    fmap id     v = v
    pure id <*> v = v

(A.2) If you have a pure function and a pure value, that's the same as lifting the result of `f v`. It's the same as F.2:
    fmap g  .  fmap f = fmap (g . f)
    pure g <*> pure f = pure (g   f)

(A.3) Swapping some value/fn `u` over the splat with a `pure y` (y is val/fn) is the same as `pure ($ y)` on LHS and `u` on RHS.

> pure ($ 5) <*> Just (+10)                                 -- Just 15
> Just (+10) <*> pure 5                                     -- Just 15
> (pure ($ 5) <*> Just (+10)) == (Just (+10) <*> pure 5)    -- True

Typeclassopedia: "Intuitively, this says that when evaluating the application of an effectful function to a pure argument, the order in which we evaluate the function and its argument doesn't matter."

https://stackoverflow.com/questions/27285918/applicatives-interchange-law
https://wiki.haskell.org/Typeclassopedia#Laws_2

```
u <*> pure y == pure (      $ y) <*> u
u <*> pure y == pure (\f -> f y) <*> u


u          <*> pure y == pure ($ y)       <*> u
                       = pure ($ 5)       <*> Just (+10)
                       = pure ($ 5) (+10)
                       = pure ((+10) $ 5)
                       = pure ((+10) 5)
                       = pure 15

u          <*> pure y == pure (\f -> f y) <*> u

Just (+10) <*> pure 5 == pure ($ 5)       <*> Just (+10)
                       = pure (\f -> f 5) <*> Just (+10)
                       = pure (\f -> f 5) (+10)
                       = pure ((+10) 5)
                       = pure 15
```


(A.4) Lifting composition over `u <*> v <*> w` makes it 
right-associative, it becomes  `u <*> (v <*> w)`




## Monad

https://wiki.haskell.org/Monad_laws

return a >>= k                 ≡ k a                M.1 LEFT IDENTITY
       m >>= return            ≡ m                  M.2 RIGHT IDENTITY
       m >>= (\x -> k x >>= h) ≡ (m >>= k) >>= h    M.3 ASSOCIATIVITY


(M.1) We can place some value `a` in the monadic context with `return a`, then bind it to a function, `>>= k`, but that's equal to applying that function directly, `k a`.
> return 5 >>= (\x -> Just (x + 5)) -- Just 10


(M.2) m >>= return == m
> return 5 >>= (\x -> Just (x + 5)) -- Just 10

(M.3) The law can be re-written for clarity as:

            (m  >>=  (\x -> f x))  >>=  g   ≡
             m  >>=  (\x -> f x    >>=  g)
or equally:
            (m  >>=  (\x -> f x))  >>=  (\y -> g y)    ≡
             m  >>=  (\x -> f x    >>=  (\y -> g y))


### Monadic laws in do-notation:

```hs
LeftIdentity = do
    x' <- return x
    f x'

LeftIdentity = do
    f x



RightIdentity = do
    x <- m
    return x

RightIdentity = do
    m



Associativity = do
    y <- do
        x <- m
    g y

Associativity = do
    x <- m
    do
    y <- f x
    g y

Associativity = do
    x <- m
    y <- f x
    g y
```


Associativity:

do { y <- do { x <- m; | ≡ do { x <- m;        | ≡ do { x <- m;
               f x     |        do { y <- f x; |        y <- f x;
             }         |             g y       |        g y
     g y               |           }           |      }
   }                   |      }                |



> let f = (\x -> Just (5 - x))
> return 5    >>= f             -- Just 0
> return (-5) >>= f             -- Just 10

> let g = (\x -> Just (10 - x))
> return 5 >>= f >>= g          -- Just 10
> return 5 >>= g >>= f          -- Just 0

> Just 5 >>= (\x -> f x >>= g)  -- Just 10
> (Just 5 >>= f) >>= g          -- Just 10



To see precisely why they're called "identity laws" and an "associative law", you have to change your notation slightly. The monad composition operator (also known as the Kleisli composition operator) is defined in Control.Monad:

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(m >=> n) x = do
    y <- m x
    n y
```

Using this operator, the three laws can be expressed like this:

- Associativity : `(f >=> g) >=> h ≡ f >=> (g >=> h)`
- Left identity : `return >=> g ≡ g`
- Right identity: `g >=> return ≡ g`
- Total identity: `return >=> g ≡ g ≡ g >=> return`

It's now easy to see that monad composition is an associative operator with left and right identities.



### A Closer Look at bind

Thanx to Category Theory, we know that (>>=) can also be expressed with `join` and `return`.

```hs
return :: (Monad m) => a -> m a
(>>=)  :: (Monad m) => m a -> (a -> m b) -> m b

-- list bind
(>>=) :: [a] -> (a -> [b]) -> [b]
[]     >>= _ = []
(x:xs) >>= f = f x ++ (xs >>= f)

-- maybe bind
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _ = Nothing
(Just x) >>= f = f x

-- join (concat)
join :: (Monad m) => m (m a) -> m a
join x = x >>= id

-- bind via join
mx >>= f = join $ fmap f mx
```
