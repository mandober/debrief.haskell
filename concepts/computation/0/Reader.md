# Reader monad

The `Reader` data type (monad) models a global, readonly environment.

## Is Reader really required?

Now, In Haskell, we might as well have a global variable that holds some configuration, and it would be accessible to all functions in the module. Since a global configuration is something that is assembled up front and immutable from then on, functions that need access to it can just declare an extra parameter to bind it, and they don't need to return it (since it is immutable anyway). A function may change the environment locally, but these changes are not meant to propagate outside that function, so there is really no need to return the environment.

Out of all the other ways that model global state, the `Reader` is specific in that (1) the environment is assembled up front, and (2) it remains immutable throughout the module. The Reader models a *read-only resource*.

On the other hand, the `Writer`, which deals with threading a log through functions, models a *write-only resource*. That log (data type that represents it) must be accepted by functions, which then modify it, and so it must be returned as well. The log goes through a set of changes - it is assembled on the go, unlike a readonly environment. Log is peculiar because it is indeed write only resource - the functions do not inspect it, they just append their bit onto it and pass it along.

The `State` data type requires both, reading and writing. It deals with a *read and write resource*. The state models a global mutable state, so the threaded resource needs both read and write permissions. Functions that accept the state may modify it, and so they must return it for the changes to propagete. The threaded state changes as it passes through functions, it is not assembled up front, but may be mutated at any moment.

However, all of these 3 things are implemented similarly by altering functions to accept an extra parameter and to return an extra value. My point is, there is no need to do that for the readonly resource, but despite that the Reader follows this scheme (complicating the things for the fun of it).

## The Reader type

Therefore, to thread a global readonly environment, we are to alter a function that normally takes some argument and return some value, into taking a pair - the original argument and the environment - however, it needs not return the environment - the origina lreturn value remain the same.


## As Kleisli arrow

As a Kleisli arrow, the Reader is a function with augmented (embellished) return type - instead of `a`, its return type becomes `m a`.

```hs
foo :: r -> a
foo :: r -> m a
```

This is not important now, for the "bare" Reader type, but it will be significant later, when we turn it into a monad transformer.

## True methods of the Reader type

The declaration of the `Reader` type just reads:

```hs
newtype Reader r a = Reader (r -> a)
```

which shows that the `Reader` type is just a function wrapper. The type parameter `r` represents the environment, and the type parameter `a` is the return value of some "original" function before we "augmented" it with the reader capability. The original input parameters of that function are not represented in the Reader type - they need not be because they can be prepended to this type anytime.

For example, if a function `fun1` has input parameters `x` and `y`, then after being made "Reader-compatible", it will just take an extra paramter `r`:

```hs
-- original function
fun1 :: x -> y -> a
-- extended to accept the env
fun2 :: x -> y -> r -> a
-- env + return type as `Reader r a`
fun3 :: x -> y -> Reader r a
-- ignoring the wrapping, it is just
fun2 :: x -> y -> (r -> a)
```

The parameter that binds the environment comes last (just before the return type) so the origianl parameters of the function can be prepended. This allows us to form the Reader type as `Reader r a`. The parameter that binds the environment, `r`, and the return type `a`, sort of "fuse" together into a new return type `Reader r a`, which expands to `Reader (r -> a)`, which in turn (ignoring the wrapping) is just `(r -> a)`.





```hs
eval0 :: Env -> Exp -> Value
eval1 :: Env -> Exp -> Identity Value
eval2 :: Env -> Exp -> ExceptT String Identity Value
eval3 :: Exp -> ReaderT Env (ExceptT String Identity) Value
```





Reader - auxillary functions (true Reader methods)

## FAM instances for Reader

- `fmap` is (.) i.e. B combinator
- `<*>` is S combinator
- `>>=` is flipped S combinator (modulo un/wrapping)

## Reader primitive functions

- Reader: we must introduce the env, i.e. type param `r`, as a lambda
- env, `r`, is consumed
- ask - get env
- asks - get and map the env
- local - modifies env locally



### ask
- Fetch the value of the environment

```hs
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
```

The hidden env is exposed as the return value `r`, which is why the env is duplicated as `Reader r r` (not `Reader r a`) - although it is not really duplicated, but fetched and returned.

### asks
- Retrieve a function of the current environment
- asks f = liftM f ask
- it uses a selector fn, `r -> a`, to extract a piece `a` from the env `r`


```hs
asks :: Monad m
     => (r -> a)         -- ^ selector function to apply to env
     -> ReaderT r m a
asks f = ReaderT (return . f)
```

### local
- Execute a computation in a modified environment
- specialization of `withReaderT`
- runReaderT (local f m) = runReaderT m . f


```hs
(*>)  :: (Monad m) => ReaderT r m a -> ReaderT r m b -> ReaderT r m b
(<*)  :: (Monad m) => ReaderT r m b -> ReaderT r m a -> ReaderT r m b
(<*)  :: (Monad m) => ReaderT r m a -> ReaderT r m b -> ReaderT r m a
(<*>) :: (Monad m) => ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
(>>=) :: (Monad m) => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b

rma <* rmb = ReaderT \ r -> runReaderT rma r <* runReaderT rmb r


ma  *> mb = ReaderT \ r -> runReaderT ma r  *> runReaderT mb r
mb <*  ma = ReaderT \ r -> runReaderT ma r  *> runReaderT mb r
mf <*> ma = ReaderT \ r -> runReaderT mf r <*> runReaderT ma r

mf <*> ma = ReaderT \ r -> runReaderT mf r <*>        runReaderT  ma    r
ma >>= mf = ReaderT \ r -> runReaderT ma r >>= \ a -> runReaderT (mf a) r
```
