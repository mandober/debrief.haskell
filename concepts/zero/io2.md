# IO

- book: Haskell progamming from the first principles
- CIS 194: https://www.cis.upenn.edu/~cis194/spring13/lectures/08-IO.html
- Bartosz Milewski: https://www.youtube.com/watch?v=h6zbQ23U05g&list=PLbgaMIhjbmEm_51-HWv9BQUXcmHYtl4sw&index=11
- http://learnyouahaskell.com/input-and-output
- http://book.realworldhaskell.org/read/io.html


`IO` is an opaque type; we can't see what it contains nor what it consists of. The IO monad is sometimes explained using van Laarhoven's Free Monads and costate comonad coalgebras. IO type has a Monad instance, but it's not the only monad and monads are certainly not only about IO. Implying that, once you enter IO, you destroy purity and referential transparency is incorrect.

`IO` is a special type (it is opaque type handled by the compiler in a special way) that inherently disallows certain types of (data) sharing, thus preventing the side-effects from percolating throughout the program.

A popular way of explaning IO is it in tems of `State`.

`State` type is an alias a function in the same way as many similar types that wrap a function with a similar signature (Reader, Writer, etc.). The docs of *GHC.IO* module also attempt to explain the `IO` type using the State approiach, saying:

> The IO Monad is just an instance of the ST monad, where the state is the real world.

Which becomes slightly clearer when we see the underlying types:

```hs
-- with type
type State s a = s -> (a, s)
type IO    w a = w -> (a, w)

-- with newtype:
newtype State s a = State { runState :: s -> (a, s) }
-- State does involve RealWorld:
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

-- but underneath it all, it's just a wraper for the fn:
f :: b -> (a, b)

-- :info IO
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

Looking at the last signature, the `State` there is a signalling mechanism for telling GHC what order the IO actions are in and what a unique IO action is. GHC.Prim documentation says:

`State#` is the primitive, unlifted type of states. It has one type parameter, thus `State# RealWorld`, or `State# 𝑠`, where `𝑠` is a type variable. The only purpose of the type parameter is to keep different state threads separate. It is represented by nothing at all (no memory usage). `RealWorld` is deeply magical; it's a primitive, but it's not unlifted. We never manipulate values of type `RealWorld`; it's only used in the type system, to parameterise `State#`.

The state tokens underlying the IO type are erased during compile time - they add no overhead to the runtime. The problem with explaining IO in terms of State is that it is imprecise: it's not a State you can meaningfully interact with or control in the way you'd expect from the other State types.


IO primarily exists to give us a way to order actions and to disable some of the sharing related to non-strictness.

GHC is ordinarily free to do a lot of reordering of operations, delaying of evaluation, sharing of named values, duplicating code via inlining, and other optimizations in order to increase performance. *The main thing the IO (and ST) type does is turn off most of those abilities*. IO actions are instead enclosed within nested lambdas (thunks). *Nesting is the only way to ensure that actions are sequenced within a pure lambda calculus*.

Nesting lambdas is the awy to guarantee that this

```hs
main = do
    putStr   "1"
    putStr   "2"
    putStrLn "3"
```

will output "123". The underlying representation of IO allows the actions to be nested, and therefore sequenced.

When we enter a lambda expression, any effects that need to be performed will we performed first, before any computations are evaluated. Then, if there is a computation to evaluate, it may be evaluated next, before we enter the next lambda to perform the next effect and so on. For example, think of the parsers that perform the effect of moving a "cursor" through the text without reducing to any value. One of the improvements that monads bought is that they abstract away the nested lambda noise underlying IO.
