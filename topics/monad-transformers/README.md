# MT

https://hackage.haskell.org/package/transformers

```hs
newtype Reader  r       a = Reader  { runReader  :: r      ->    a }
newtype ReaderT r     m a = ReaderT { runReaderT :: r      -> m  a }

newtype Writer    w     a = Writer  { runWriter  ::             (a, w) }
newtype WriterT   w   m a = WriterT { runWriterT ::           m (a, w) }

newtype State       s   a = State   { runState   ::      s ->   (a, s) }
newtype StateT      s m a = StateT  { runStateT  ::      s -> m (a, s) }

newtype RWS     r w s   a = RWS     { runRWS     :: r -> s ->   (a, s, w) }
newtype RWST    r w s m a = RWST    { runRWST    :: r -> s -> m (a, s, w) }
```

## Embellishing a type

```hs
a -> b            Generalized function type
a -> m        b   Kleisli arrow
a -> List     b   Models indeterminism
a -> Maybe    b   Models fallability (no error message)
a -> Either e b   Models fallability (with an error message)
a -> (, w)    b   Models logging,       (b, w)
a -> (, s)    b   Models state,    a -> (a, s)
```


## Reader

The Reader monad has a fixed environment type of type `r`, from which it reads member component's values of type `a`, `r -> a`. It is just a plain old function wrapped in a new type. The `Reader` is just a type that wrappes a function type, `a -> b`. However, the semantics of the Reader type suggest that the input type is fixed.

Generalized into a monad transformer, `Reader` becomes `ReaderT` which wraps the return type in a monadic type. So insead of `a -> b` we have `a -> m b`, which is the type of the Kleisli arrow. It is function whose return type is "embellished". If the monadic type is a list then we'd have `a -> [b]`.

The Reader type is more like an enhancement of a single type `a`, then of some function type. It seems that we first had a single type `a` that was a type of some payload. But the Reader introduces a fixed environment of type `r`, which is a compound type consisting of many member types. The env is then used as the source of data. The Reader becomes a function with a fixed input type, while its output type `a` varies as neeed in accordance with the types of components. The input type `r` will be some kind of compound type, really consisting of a number of nested types, e.g. `Map k (Set (a, [b]))`, so we can imagine that `a` will take on the types `k`, `a`, `b`, `[b]`, `(a, [b])`, `Set (a, [b])`, etc.

```hs
x :: a
r :: r -> a
```

## Writer

The Writer monad is about threading a log file through fucntions without resorting to declaring an extra parameter (to bind that log) in every function. The Writer type is a mere pair `(a, w)`, where `w` type param stands for the type of the log, and `a` is the type of output that is produced regularly. So if a function takes its regular input of type `a` and returns its reular output of type `b`, its signature is `a -> b`. Now if we enhance this function so it records some info in a log of type `w`, it means that its signature will have to be adjusted, `(a, w) -> (b, w)`. It now takes and returns a pair.

The `Writer` and `State` types are very similar: both have their input and output type embellished. The `Writer` type, unlike the `State` type, however does not embed the (new) input type, `(a, w)`, in the `Writer` type, only the output type `(b, w)`:

```hs
type Writer w a = Writer (w, a)
```

If we express a function's signature in terms of plain types (as expanded Writer type), there will surely be a type that binds the function regular input; and now, there will also have to be a type that binds the logging objects. These two may be expressed as a single parameter `(a, w)`, or, equivalently, as two parameters, `a` and `w`. The return type is `(b, w)`. All together now:

```hs
writer :: a -> b
writer :: (a, w) -> (b, w)
writer :: a -> w -> (b, w)
writer :: a -> w -> Writer b
```

The Writer abstracts only the return type `(b, w)`, unlike the `State` type which abstracts the return type along with one of the input parameters. Here's the derivation using a type variable `w` to represent the state object:

```hs
state :: a -> b
state :: (a, w) -> (b, w)
state :: a -> w -> (b, w)
state :: a -> State w b
```

`Writer` vs `State` abstraction comparision:

```hs
writer :: a -> w -> (b, w)
writer :: a -> w -> Writer b
state  :: a -> w -> (b, w)
state  :: a -> State w b
```

Therefore, the `Writer` type is defined as:
```hs
newtype Writer w a = Writer (     (a, w))
```

while the `State` type is defined as:
```hs
newtype State  s a = State  (s -> (a, s))
```

Side-by-side and using the same (`w`) for the payload's type param:

```hs
newtype Writer w a = Writer (     (a, w))
newtype State  w a = State  (w -> (a, w))

-- considered as plain function types,
-- there is no difference between the two:
writer :: a -> w -> (b, w)
state  :: a -> w -> (b, w)
```

The `Writer` type is basically just another name for the return pair type.

The `State` type is another name for the return pair type as well, but it also encompasses the last input parameter, giving a new name to the last part of the signature, `… -> s -> (a, s)`.

```hs
-- an arbitrary signature that almsot conforms to the State signature.
-- even if `s` is not the last param, usually we can rearrange it:
foo :: a -> s -> (a -> b) -> Set b -> (a, s)
-- a signature that conforms to the State signature:
foo :: a -> (a -> b) -> Set b -> s -> (a, s)
-- now it can be rewritten in terms of the State type
foo :: a -> (a -> b) -> Set b -> State s a
```

>The input type that represents the state object gets abstracted along with the output type that encompasses both the updated state object and the usual return type.


## State

```hs
newtype State       s   a = State   { runState   ::      s ->   (a, s) }
newtype StateT      s m a = StateT  { runStateT  ::      s -> m (a, s) }
```

How do we model a state in a pure language? We thread the state object through functions, so each function will take a state along with its regular input; it will also return the updated state along with its regular output. So, both input and output type of a general function `a -> b` get embellished with an addition, state type `s`, so the signature becomes `(a, s) -> (b, s)`.



```hs
f :: a -> b
s :: (a, s) -> (b, s)
s :: a -> s -> (b, s)
```



This resables the Writer very much, except the Writer never reads the log object, `w`; it only writes to it by appending its own message. On the other hand, the `State` needs both read and write access to the state object. A function 
