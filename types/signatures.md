

Signatures of functions are often stated, even though they are not required. However, not only it is a good style to write them, but sometimes being explicit about the type prevent GHC from inferring more general types (GHC will always infer as broad as type as posible).

Any function should fit this signature: `a -> b`. However this is not the case by default because GHC does not allow kinds on type parameters, meaning `a` or `b` can only be the types of kind `*`. So, the type params here may be `Int` or even any function type, but not n-ary type ctors (with n>1), like `[a]` or `Maybe a`.

With language extensions kinds on type parameters can be enabled alowing `a` to stand for a higher-kinded type, like `Maybe a` that has a kind `* -> *`, or `Either a b` whose kind is `* -> * -> *`, or even `(,,,,) :: * -> * -> * -> * -> * -> *`. The higher-kinded champion seems to be a 62-tuple ctor with a sequence of 61 commas (after that GHC complains suggesting making the nested tuples).


bind sig: `(>>=) :: Monad m => m a -> (a -> m b) -> m b`



```hs
--    op        context           arg1            arg2          return
     (<$>) :: (Functor     f) =>   (a ->   b)  -> f a           -> f b
     (<*>) :: (Applicative f) => f (a ->   b)  -> f a           -> f b
flip (<$>) :: (Functor     f) => f a           ->   (a ->   b)  -> f b
flip (<*>) :: (Applicative f) => f a           -> f (a ->   b)  -> f b
     (>>=) :: (Monad       f) => f a           ->   (a -> f b)  -> f b
flip (>>=) :: (Monad       f) =>   (a -> f b)  -> f a           -> f b
     (=<<) :: (Monad       f) =>   (a -> f b)  -> f a           -> f b
     (>> ) :: (Monad       f) => f a           -> f b           -> f b
     (<* ) :: (Applicative f) => f a           -> f b           -> f a
     ( *>) :: (Applicative f) => f a           -> f b           -> f b
```
