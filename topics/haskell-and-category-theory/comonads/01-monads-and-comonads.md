# Monads and comonads

Monads operate on values in a context, they are *Kleisli arrows*, `a -> m b`, from a pure value `a` to an embellished, monadic value `b`, i.e. a value `b` that lives in a monadic context `m`.

By flipping a Kleisli arrow, we get a *co-Kleisli arrow* from a value in a comonadic context to a pure value, `w a -> b`. Comonads operate on values that live in a context which they can refer to - the context is often consulted to calculate the value that is then extracted.

In general, each monad provides a way to get a pure value into a monadic context, but there's no way to get the value out. The `IO` monad offers no way to get the value out (except using the unsafe, "escape hatch", methods that go around the type system, like `unsafePerformIO`). However, some monads do provide ways to get the value out, but this is not a specification of the general monadic interface.

Comonads are the dual of monads - they provide no way to get the values in, but allow us to get values out. Similarly to some monads that offer ways (intrinsic to each monad) to get values out, some comonads have ways (intrinsic to each comonad) to get values in.




## Comonad definition

Monads can be defined in two ways: in Haskell, they are defined using `return` and bind (`>>=`), while category theorists prefers the definition in terms of `fmap` and `join` (`fmap` is available because every monad is a subclass of the `Functor` class, and the same is true for the `Comonad` class), and the return is called a unit (natural trasformation denoted by `η`).

```hs
class Monad m where
  -- aka unit, η
  return :: a -> m a

  join  :: m (m a) -> m a
  join = bind' id

  -- flip bind
  (=<<) :: (a -> m b) -> (m a -> m b)
  (=<<) f = join . fmap f

  -- aka bind
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) ma f = join (fmap f ma)

  -- flip (<=<)
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  (>=>) f g = join . fmap g . f          -- vs (g . fmap f . duplicate)

  -- flip (>=>)
  (<=<)  :: (a -> m b) -> (b -> m c) -> (a -> m c)
  (<=<) f g = (=<<) g . f
```

Comonads have the counit, which is natural trasformation denoted by `ϵ`, or the `extract` method in Haskell. In category theory, a comonad's definition uses the dual of the `join`, called `duplicate`, and `fmap`. Haskell's definition instead uses the dual of bind, called `extend`.




## Kleisli

For monads, the Kleisli arrow `a -> m a`, called `return` in Haskell and `η` (unit) in category theory (where it is considered a natural transformation), allow us to get a pure value into the monad. Its comonadic dual is the co-Kleisli arrow, in Haskell called `extract` and `ϵ` (co-unit) in category theory (where it is also a natural transformation), that allows us to get a pure value out of the comonad.

```hs
class Monad m where
  return :: a -> m a

class Comonad w where
  extract :: w a -> a
```

Each monad defines its own way of operating on values, but the names of the main operations are gathered together under as the methods of the `Monad` class. It is similar for comonads and the `Comonad` class.

There are two equivalent ways to define a monad, but both share the `return` (`η`) method. Haskell defines it by additionally using the bind (`>>=`) operator, while category theory prefers the definition in terms of `fmap` and `join`.

```hs
-- an instance of the Monad class has available:
-- Functor's methods:     (<$>), ($>), (<$) and also
-- Applicative's methods: (<*>), (<*), (*>), pure, liftA2
fmap :: (Functor m) => (a -> b) -> m a -> m b
(<$) :: (Functor m) => (a -> b) -> m a -> m b
(<$) :: (Functor m) =>       b  -> m a -> m b -- flip ($>)
($>) :: (Functor m) => m a -> b        -> m b -- flip (<$)

pure   :: (Applicative m) => a -> m a
(<*>)  :: (Applicative m) => m (a -> b) -> (m a -> m b)
(<*)   :: (Applicative m) =>        m b -> (m a -> m b) -- flip (*>)
(*>)   :: (Applicative m) => m a -> m b         -> m b  -- flip (<*)

liftA2 :: (Applicative m) => (a -> b -> c) -> (m a -> m b -> m c)

return :: (Monad m)       => a -> m a -- (pure)
return = pure



class Functor m => Monad m
class Functor w => Comonad w
```




In Haskell, this class is defined using unit (`return`) and bind (`>>=`), but it may also be defined, 


from a pure value `a` to an embellished value `b`, i.e. to a value `b` that is embellished in a monadic context `m`.







The forward composition, (`>=>`), allow us to compose such Kleisli arrows, which involves doing some more work to make the composition of these incompatible arrows work.


```hs
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
-- (1)
f >=> g = join . fmap g . f -- vs (g . fmap f . duplicate)
-- (2)
f >=> g = (=<<) g . f
```





the method of the `Monad` class called `return`.

how are the Kleisli arrows composed, just as it defines its own ways of getting a pure value into a monad - gathering all these ways under the method of the `Monad` class called `return`.

```hs
return :: Monad m => a -> m a
```
