# Mutable objects

https://en.wikibooks.org/wiki/Haskell/Mutable_objects

## Functional purity

*Functional purity* is a defining characteristic of Haskell, one which leads to many of its strengths. As such, language and ecosystem encourage eschewing *mutable state* altogether. Thanks to tools such as the State monad, which allows us to keep track of state in a convenient and functionally pure way, and efficient *immutable data structures* like the ones provided by the containers and unordered-containers packages, Haskell programmers can get by perfectly fine with complete immutability in the vast majority of situations.

However, under select circumstances using *mutable state* is the most sensible option. One might, for instance, be interested in:
- From Haskell code, using a library written in another language which assumes mutable state everywhere. This situation often arises with event-callback GUI toolkits.
- Using Haskell to implement a language that provides imperative-style mutable variables.
- Implementing algorithms that inherently require destructive updates to variables.
- Dealing with volumes of bulk data massive enough to justify squeezing every drop of computational power available to make the problem at hand feasible.

Any general-purpose programming language should be able to deal with such tasks. Haskell is no different: there are not only ways to create mutable objects, but it is possible to do it in such way to keep mutability under control in a setting where immutability is the default.


## Mutable values

A common way of structuring code for UIs is through the *event-and-callback model*. The event might be a button click or a key press, while the callback is just a piece of code meant to be called in response to the event. The client code should set up the wiring that connects interface elements, events involving them, and the corresponding callbacks. A hypothetical function to arrange a callback might have the following type:

```hs
register :: (Element -> Event) -> Element -> IO () -> IO ()
```

The `IO ()` argument is the callback, while the result is an IO action which sets up the wiring. Running `register click button1 (print "Hello")` would lead to "Hello" being printed following a click on `button1`.

Both `register` − with pervasive IO and lacking useful return values − and our exposition above have a marked imperative feel. That's because our hypothetical GUI library was written using a more imperative style in a wholly different language. To use from Haskell we do it through an interface, which is a thin facade so the style of the original library leaks into the code.

The technical term for *facade* over a library from other languages is **bindings**. Bindings can be thin, exposing transparently the constructs of the original library, or they can add extra layers of abstraction to achieve a more Haskell-like feel. The elementary tool for creating bindings in Haskell is the foreign function interface (FFI).

Using `register` to perform IO actions is easy enough. However, what if we want to add 1 to a counter every time a button is clicked? The type of `register` doesn't reveal any way to pass information to the callback, nor to get information back from it (the return types are units). State also does not help: even if there was a way to pass an initial state to the callback, run a stateful computation within it, what would we do with the results? We would need to pass the resulting state of the counter to the callback next time the button is clicked, and we would have no idea when that would happen, nor a place to keep the value in the meantime.

The obvious solution to this issue in many languages would be to creae a mutable variable outside of the callback and then give the callback a reference to it, so that its code can change the value of the variable at will.

However Haskell also allows us to do just that. In fact, there are several types of mutable variables available, the simplest of which is the `IORef`.

## IORef

The type `IORef`, from the `Data.IORef` module, are cells that hold references to mutable values. IORef comes with several "primitive" operations on it:

```hs
newIORef   :: forall a. a -> IO (IORef a)
readIORef  :: forall a. IORef a -> IO a
writeIORef :: forall a. IORef a -> a -> IO ()
```

The `newIORef` takes a value and returns - as the result of an IO action - an IO reference initialised to that value.

```hs
GHCi> import Data.IORef
GHCi> box <- newIORef (4 :: Int)
```

We use `readIORef` to retrieve the contained value.

```hs
GHCi> readIORef box >>= print
```

`modifyIORef` updates the contained value, and `writeIORef` just writes a value to the cell immediately.

```hs
GHCi> modifyIORef box (2 *)
GHCi> readIORef box >>= print -- 8
GHCi> writeIORef box 0
GHCi> readIORef box >>= print -- 0
```
