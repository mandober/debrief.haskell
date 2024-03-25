# Haskell :: Index :: Class laws

- Associativity
- Identity
  - Left Identity
  - Right Identity
- Composition
- Homomorphism
- Interchange



Semigroups
- x <> y <> z  ≡  (x <> y) <> z  ≡  x <> (y <> z)       SG1A. ASSOCIATIVITY

Monoids
- x <> ϵ  ≡  x  ≡  ϵ <> x                               AA3. TOTAL IDENTITY

Functors
-         fmap id  ≡  id                                F.1 IDENTITY
- fmap g . fmap f  ≡  fmap (g . f)                      F.2 COMPOSITION

Applicative
- pure        id <*>       x  ≡  x                      A.1 IDENTITY
- pure         f <*> pure  x  ≡  pure (f x)             A.2 HOMOMORPHISM
-              f <*> pure  x  ≡  pure ($ x) <*> f       A.3 INTERCHANGE
- pure (.) <*> f <*> g <*> x  ≡  f <*> (g <*> x)        A.4 COMPOSITION

Monad
- return a  >>= k                  ≡  k a               M.1 LEFT IDENTITY
-        m  >>= return             ≡  m                 M.2 RIGHT IDENTITY
-        m  >>= (\x -> k x >>= h)  ≡  (m >>= k) >>= h   M.3 ASSOCIATIVITY

Monad composition
- return >=> g ≡ g                                      K.1 LEFT IDENTITY
- g >=> return ≡ g                                      K.2 RIGHT IDENTITY
- return >=> g ≡ g ≡ g >=> return                       K.0 TOTAL IDENTITY
- (f >=> g) >=> h ≡ f >=> (g >=> h)                     K.3 ASSOCIATIVITY


## Show

## Read

## Enum

## Bounded

## Semigroup

## Monoid

## Functor

## Applicative



## Monad
