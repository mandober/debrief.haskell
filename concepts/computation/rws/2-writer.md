# Writer

```hs
newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }
```

The data type of the `WriterT` monad transformer is based on the `Writer`, which is just a glorified pair. The `WriterT` adds the slot `m` so that another monad may be plugged in in order to be composed with `WriterT`. WriterT by itself is a monad transformer, but composed with another monad we get a transformed writer together with the capabilities of that other monad.


## Comparing Writer an WriterT

```hs
newtype Writer  w   a = Writer  { runWriter  ::   (w, a) }
newtype WriterT w m a = WriterT { runWriterT :: m (w, a) }
```

The slot for the other monad `m` is in the application position - it makes the monad `m` be applied to the pair `(w, a)`, as `m (w, a)`.




## The WriterT monad transformer

https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Writer-CPS.html

`data WriterT w m a`

The `WriterT` monad transformer is parameterized by:
- `w` output to accumulate
- `m` the inner monad

The `return` function produces the output `mempty`, while `m >>= k` combines the outputs of the subcomputations using `mappend` (aka `<>`):

```
m >>= k

╭─╮          ╭─╮
│m├─────────→┤k├──────→
╰┬╯          ╰┬╯
 │            ↓
 ╰──────────→ <> ─────→ w
```
