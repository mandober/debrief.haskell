# Monad laws

Instances of the `Monad` class must satisfy the following laws:

1. return a >>= k           =  k a                 [Monad-1-IdLeft]
2. m >>= return             =  m                   [Monad-2-IdRight]
3. m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h     [Monad-3-ASSOC]


Furthermore, the Monad and Applicative operations should relate as follows:

pure = return
m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))
The above laws imply:

fmap f xs  =  xs >>= return . f
(>>) = (*>)
and that pure and (<*>) satisfy the applicative functor laws.



## Monad composition

- return >=> g ≡ g                                      K.1 LEFT IDENTITY
- g >=> return ≡ g                                      K.2 RIGHT IDENTITY
- return >=> g ≡ g ≡ g >=> return                       K.0 TOTAL IDENTITY
- (f >=> g) >=> h ≡ f >=> (g >=> h)                     K.3 ASSOCIATIVITY
