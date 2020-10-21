# Signatures

Looking at the lambda abstraction, nickname "Cardinal", `λabc.bac` and the SK-Combinator "C" defined as: `C a b c = b a c`, we might recognize some swappin' goin' round taking blame. How would we express this switcharoo in Haskell? Concretely, how would we implement the flip function that takes a binary function and returnes a new function that operates on the args in reversed order?

It ain't f-utile to outline the known facts:
- `flip` must be a higher-order function
- `flip` has got to declare at least one param
- that param takes a binary function `flip` is gonna massage into submission
That work consists of returning a new function, pretty much the same as the original one, only when this new fn gets fed its args, unlike the original, it will use them args in reverse order.

The swap can happen in two locations: in the params list or in the body of the function, where the args are used.


```hs
-- EXAMPLE 1
-- Actually, this variant won't work:
pairing :: a -> b -> c
pairing x y = (x, y)
-- GHC won't match the expected type c with the actual type (a,b)
-- it seems the sig must be written as:
pairing :: a -> b -> (a, b)
pairing x y = (x, y) -- the impl remains the same

-- EXAMPLE 2
-- With the tuple sig the failure persists, even with the same message
pairing :: (a, b) -> c
-- GHC won't match the expected type c with the actual type (a,b)
-- Writting the sig like this works:
pairing :: (a, b) -> (a, b)
-- hence the impl is:
pairing (x, y) = (x, y)
-- but, if that's the case, we can rewrite it as:
pairing = id
```

-------------------------------------------------------------------------------




What follow form this in the setting of a strongly-typed language, is the fact that these two args must have the same type since they are swappable with each other. In fact, they may have a different type after all, but in that case we know such function doesn't relly do any heavy pulling; it can only manipulate those args without knowing what they actually are. One of the plausable scenarios is that the orignal function is a pairing fn that just places the args, as they come, in a tuple, `(x, y)`. So its "swap" version will end up producing `(y, x)` instead. There are other options that either of the two functions may explore, but they are heavily constrained by the parametricity.

In any case, whether we have a binary function (a -> a) or (a -> b), we'll just assume the more general version with (posssibly) different args. After all, any fn with the sig (a -> a) unifies with the sig (a -> b) but not vice versa.

However, we have a binary function and in most general case that looks like 
`binary :: (a -> b -> c)`.




Now, the signature of a binary fn, without knowing anything else, must be as general as possible. Similarly, the sig for unary fn about which we know nothing, should be ultra general, i.e. (a -> b). This sig (which I think no Haskell function has) is the most general signature "placeholder" for function types because the only thing we can conclude from it is that it's a function.

Considering only functions, any function but also any type as well, will fit into the signature mold `a`. Everything fits in `a` and so does all functions types, for `a` could mean, e.g. (b -> b) or (b -> c -> d) or just about anything else. So, the sig `a` tell us absolutely nothing, but with (a -> b) we know it's a function at least. Apart from that we can't assume anything else about it because it doesn't even insist that the two types are different, `b` might end up being the same type as `a`. To insist on the same types, we would instead write (a -> a) as the sig.

Also, nothing here implies the arity of the function, no matter that (a -> b) tends to invoke unary feelings; it might fit a binary fn as well, (c -> d -> e) with `c -> d` unifying into `a` and `e` into `b`.




-------------------------------------------------------------------------------
swapargs :: (a -> b -> c)

- and the Haskell's flip :: (a -> b -> c) -> b -> a -> c

we must never loose out of sight the fact that lambda and SK expressions are values, but we have a fn type signature in Haskell's case.


flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


Bluebird, (.) :: λgfx.g(fx)
Cardinal :: λabc.acb

flip :: λa.λb.λc.bac
flip :: (a -> b -> c) -> a -> b -> c
flip :: (a -> b -> c) -> b -> a -> c
              f          y    x    z
f(x,y) = z ~~> f(y,x) = z

-------------------------------------------------------------------------------



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
