# Monads

```hs
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    fmap :: (a -> b) -> (m a -> m b)
    join :: m (m a) -> m a
{-# MINIMAL return, ( (>>=) | (fmap|join) ) #-}


p >>= k  == join (fmap k p)
join pp  == pp >>= id
fmap f p == p  >>= (return ◦ f )

-- LAWS
return x >>= k = k x                        -- left unit
p >>= return = p                            -- right unit
(p >>= h) >>= k = p >>= (λx -> h x >>= k)   -- associativity

fmap id = id                                -- map-identity
fmap (f . g) = fmap f . fmap g              -- map-composition

join . return      = id                     -- left unit
join . fmap return = id                     -- right unit
join . fmap join = join . join              -- associativity
```


## Monad laws

https://wiki.haskell.org/Monad_laws

Law            | LHS₁      | ⨠= | LHS₂   | ≡ | RHS
---------------|-----------|-----|--------|---|-------------------------
Left identity  | return a  | ⨠= | h      | ≡ | h a
Right identity | m         | ⨠= | return | ≡ | m
Associativity  | (m ⨠= g) | ⨠= | h      | ≡ | m ⨠= (λx → g x ⨠= h)

$$
\begin{align}
return\ a           &\ ⨠\!\!\!\! = h      &≡&\ h\ \ a \\
m\                  &\ ⨠\!\!\!\! = return &≡&\ m\   \\
(m\ ⨠\!\!\!\! = g) &\ ⨠\!\!\!\! = h      &≡&\ m\ ⨠\!\!\!\! =\ (λx\ \to\ g\ \ x\ ⨠\!\!\!\! =\ h)
\end{align}
$$

### Monad associativity law

Using eta-expansion, the associativity law can be re-written for clarity as:

```hs
(m  >>=         g)    >>=  h             ===
 m  >>=  (\x -> g x   >>=  h)

(m  >>=  (\x -> g x)) >>=  h             ===
 m  >>=  (\x -> g x   >>=  h)

(m  >>=  (\x -> g x)) >>=  (\y -> h y)   ===
 m  >>=  (\x -> g x   >>=  (\y -> h y))
```
Or

```hs
(m >>=        g)    >>=        h    === m >>= (\x -> g x >>=        h)
(m >>= (\x -> g x)) >>=        h    === m >>= (\x -> g x >>=        h)
(m >>= (\x -> g x)) >>= (\y -> h y) === m >>= (\x -> g x >>= (\y -> h y))
```

Or unparenthesized

```hs
mx >>=       mf   >>=       mg    ===  mx >>= \x -> mf x >>=       mg
mx >>= \x -> mf x >>=       mg    ===  mx >>= \x -> mf x >>=       mg
mx >>= \x -> mf x >>= \y -> mg y  ===  mx >>= \x -> mf x >>= \y -> mg y

mx >>= \x -> mf x >>= \f -> return (f x)
-- in do-notation
do
x <- mx
f <- mf
return $ f x
```

### Monad laws and do-notation

```hs
-- left id
do { x' <- return x;  ===  do { f x }
     f x'
   }

-- right id
do { x <- m;   ===  do { m }
     return x
   }

-- assoc
do { y <- do { x <- m; | === | do { x <- m;      | === | do x <- m   |
               f x     |     |    do { y <- f x; |     |    y <- f x |
             }         |     |       g y         |     |    g y      |
     g y               |     |    }              |     |             |
   }                   |     | }                 |     |             |

-- as one-liners
do { y <- do { x <- m; f x; } g y }
do { x <- m; do { y <- f x;   g y } }
do { x <- m;      y <- f x;   g y }    -- m >>= \x -> f x >>= \y -> g y

-- as one-liners
(m >>= (\x -> f x)) >>= \y -> g y   -- do { y <- do { x <- m; f x; } g y }
 m >>= (\x -> g x >>= (\y -> h y))  -- do { x <- m; do { y <- f x;   g y } }
 m >>=  \x -> f x >>= \y -> g y     -- do { x <- m;      y <- f x;   g y }
```


### Examples

```hs
-- LEFT IDENTITY
--   return a >>= h    <=> h    a
x1 = return 5 >>= Just --- Just 5


-- RIGHT IDENTITY
--   m      >>= return <=> m
x2 = Just 5 >>= return --- Just 5


-- ASSOC

m = Just 5
g = \x -> Just (x + 3)
h = \x -> Just (x * 5)

-- ASSOC: (m >>= g) >>= h  <=>  m >>= (\x -> g x >>= h)
x3 = (m >>= g) >>= h         -- Just 40
x4 = m >>= (\x -> g x >>= h) -- Just 40

-- ex1
ex1 = do
  line <- getLine
  return line
-- same as
ex1 = getLine
-- same as
ex1 = getLine >> return

-- ex2
ex2 = do
  unused <- getLine
  line   <- getLine
  return line
-- same as
ex2 = do
  unused <- getLine
  getLine
-- same as
ex2 = getLine >> getLine
```
