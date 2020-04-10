# Monads

- Types, Type constructors, Data constructors
- Function types, Signatures
- Motivation for monads


https://en.wikipedia.org/wiki/Monad_(functional_programming)


### Motivation

If a computation might fail and we're not interested in the error message, in many PL it is customary to return a Null value. For instance, in C, a function that parses a string into a number, either returns an integer or it fails (if the input string contains anything other then digits). The common way to mark failure in such situation is with *in-band signaling*, where an otherwise valid value of the type is arbitrarily chosen to signal adversity. For example, appointing -1 as the error condition. In other cases a null value is often used to signal failure, meaning the client code must make sure the value it has received back from the function is not a null.

## Maybe type

The suitable type for this role is a `Maybe` (Haskell) also called `Option` (Rust), whih can encode both outcomes: it can either hold the payload or signal failure by being empty.

In Haskell, `Maybe` is an algebraic sum type. It has the nullary data constructor called `Nothing` and the unary data constructor called `Just`. We can examine a maybe value using pattern matching: if the value conforms to the `Nothing` pattern we know we're dealing with a failed computation, whereas it conforming to the `Just x` pattern, guarantees that there is a valid value inside (we can access it as `x`).

```hs
opt1 = Just 5 :: Maybe Int
opt2 = Nothing :: Maybe Int
opt = if Cond then opt1 else opt2
case opt of
  Nothing -> putStrLn "No value."
  Just age -> putStrLn $ "Value: " ++ show x
```

## Constructors

These ctors are, in a way, just markers of the kind of value that is returned; they tag (flag) the returned value, giving an unambiguous signal whether the process went fine or not. In case it went fine (determined bypattern matching), you can get at the payload; in case it didn't, you can examine the error.
