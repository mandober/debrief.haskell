# Applicative laws

Applicative
- pure        id <*>       x  ≡  x                      A.1 IDENTITY
- pure         f <*> pure  x  ≡  pure (f x)             A.2 HOMOMORPHISM
-              f <*> pure  x  ≡  pure ($ x) <*> f       A.3 INTERCHANGE
- pure (.) <*> f <*> g <*> x  ≡  f <*> (g <*> x)        A.4 COMPOSITION
