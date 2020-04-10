# Side-effects

- https://stackoverflow.com/questions/3850368/how-do-functional-languages-model-side-effects
- https://stackoverflow.com/questions/2158050/what-are-the-alternative-of-monads-to-use-io-in-pure-functional-programming/2158637

Having the `World` type that represents external state is a common technic used by pure functional programming languages to model side effects.

For a program to remain consistent, a value of `World` type cannot be used more then once otherwise it would be easy to make a tuple containing the same World. Forking the world must be prohibited or it would be possible to have two copies of the same world, with a value that is true in one but false in the other world (state).

---

The `World` type represents external state and the type system must guarantee that its values are only ever used once.

A language employing this approach would have functions that deal with the input and output, like "print" and "read", constrained to these signatures:

```hs
print :: (String, World) -> World
read  :: World -> (String, World)

-- USAGE:

let main w =
    let w1 = print ("What's your name?", w) in
    let (name, w2) = read w1 in
    let w3 = print ("Your name is " ^ name, w2) in
    w3

-- This would be illegal because the world is used twice
let main w =
    let w1 = print ("What's your name?", w) in
    let (name, w2) = read w in
    let w3 = print ("Your name is " ^ name, w2) in
    w3
```

All built-in functions with side-effects would take and return a world value. Since all functions with side-effects are either built-ins or call other functions with side-effects, this means that all functions with side-effects need to take and return a world.

This way it is not possible to call a function with side-effects twice with the same arguments and referential transparency is not violated.

2) An IO monad where all operations with side effects have to be executed inside that monad.

With this approach all operations with side effects would have type io something. For example print would be a function with type string -> io unit and read would have type io string.

The only way to access the value of performing operation would be to use the "monadic bind" operation (called >>= in haskell for example) with the IO operation as one argument and a function describing what to do with the result as the other operand.

The example from above would look like this with monadic IO:

let main =
  (print "What's your name?") >>=
  (lambda () -> read >>=
  (lambda name -> print ("Your name is " ^ name)))
