# Monad class

Monad class can be defined as a type constructor `m` and a pair of polymorphic operations `(>>=)` and `return` which satisfy a set of laws. The intuitive idea is that a monad `m` encapsulates a notion of computation, and `m a` can be considered as a computation `m` returning a value of type `a`.

```hs
return :: a → m a
(>>=)  :: m a → (a → m b) → m b
```

The monadic method return is equal to applicative's pure - it takes a value and puts it in a minimal default context that still holds that value.


## Define Functor and Applicative in terms of Monad class

When you have a type for which you have only defined a Monad instance, you can't use it until you also define Monad's superclasses, Functor and Applicative, for that type. To do this quickly:
- `import Control.Monad`
- for Functor:
  fmap = liftM
- for Applicative in terms of Monad:
  - pure = return
(<*>) = ap
- for Monad 
