# Imperative Functional Programming
Simon Peyton Jones, Philip Wadler, 1993

## Abstract

We present a new model, based on monads, for performing input/output in a non-strict, purely functional language. It is composable, extensible, efficient, requires no extensions to the type system, and extends smoothly to incorporate mixed-language working and in-place array updates.

## 1. Introduction

Input/output has always appeared to be one of the less satisfactory features of purely functional languages: fitting action into the functional paradigm feels like fitting a square block into a round hole. Closely related difficulties are associated with performing in-place update operations on arrays, and calling arbitrary procedures written in some other (possibly side-effecting) language.

Some mostly-functional languages, such as Lisp or SML, deal successfully with input/output by using *side effects*. We focus on purely-functional solutions, which rule out side effects, for two reasons. Firstly, the absence of side effects permits unrestricted use of *equational reasoning* and *program transformation*. Secondly, we are interested in non-strict languages, in which the order of evaluation (and hence the order of any side effects) is deliberately unspecified.

>Laziness and side effects are fundamentally inimical.

There is no shortage of proposals for input/output in lazy functional languages, some of which we survey later, but no one solution has become accepted as the consensus. This paper outlines a new approach based on monads (Moggi [1989], Wadler [1992], Wadler [1990]), with a number of noteworthy features.

* It is composable. Large programs which engage in I/O are constructed by gluing together smaller programs that do so. Combined with higher-order functions and lazy evaluation, this gives a highly expressive medium in which to express I/O performing computations - quite the reverse of the sentiment with which we began this section. We compare the monadic approach to I/O with other standard approaches: *dialogues* and *continuations*, and *effect systems* and *linear types*.

* It is easily *extensible*. The key to our implementation is to extend Haskell with a single form that allows one to call an any procedure written in the programming language C (Kernighan & Ritchie [1978]), without losing referential transparency. Using it programmers can readily extend the power of the I/O system, by writing Haskell functions which call operating system procedures.

* It is *efficient*. Our Haskell compiler has C as its target code. Given a Haskell program performing an I/O loop, the compiler can produce C code which is very similar to that which one would write by hand.

* Its efficiency is achieved by applying simple *program transformations*. We use *unboxed data types* (Peyton, Launchbury [1991]) to expose representation and order-of-evaluation detail to code-improving transformations, rather than relying on ad hoc optimisations in the code generator.

* It extends uniformly to provide *interleaved I/O* and *reference types*.

* It extends uniformly to support *incremental arrays with in-place update*. Our implementation is efficient enough that we can define monolithic Haskell array operations in terms of incremental arrays. Hudak have proposed a similar method based on continuations. Our method is more general than his in the following sense: monads can implement continuations, but not the converse.

* It is based (only) on the *Hindley-Milner type system*. Some other proposals require linear types or existential types; ours does not.

We have implemented all that we describe in the context of a compiler for Haskell, with the exception of the extension to arrays and reference types. The entire I/O system provided by our compiler is written in Haskell. The language's standard *Dialogue interface for I/O* is supported by providing a function to convert a `Dialogue` into our `IO` monad. We do not claim any fundamental expressiveness or efficiency which is not obtainable through existing systems, except where arrays are concerned. Nevertheless we feel that the entire system works particularly smoothly as a whole, from the standpoint of both programmer and implementor.

## 2. Overview

>We need a way to reconcile *being* with *doing*: an expression in a functional language denotes a value, while an I/O command should perform an action.

We integrate these worlds by providing a type `IO a` denoting actions that, when performed, may do some I/O and then return a value of type `a`. The following provide simple Unix-flavoured I/O operations.

```hs
getcIO :: IO Char
putcIO :: Char -> IO ()
```

Here `getcIO` is an action which, when performed, reads a character from the standard input, and returns that character; and `putcIO a` is an action which, when performed, writes the character `a` to the standard output. Actions which have nothing interesting to return, such as `putcIO`, return `()`.

>Notice the distinction between an action and its performance. Think of an action as a "script", which is performed by executing it. Actions themselves are first-class citizens.

How, then, are actions performed?

In our system, the value of the entire program is a single (perhaps large) action, called `mainIO`, and the program is executed by performing this action. For example, the following is a legal Haskell program

```hs
mainIO :: IO ()
mainIO = putcIO '!'
```

This is the point at which "being" is converted to "doing": when executed, the `putcIO` action will be performed, and write an exclamation mark to the standard output.


### 2.1 Composing I/O operations


## 3. Comparison with other I/O styles

In this section we briefly compare our approach with two other popular ones, dialogues and continuations.

### 3.1 Dialogues

The I/O system specified for the Haskell language is based on *dialogues*, also called *lazy streams* (Dwelly [1989], O'Donnell [1985], Thompson [1989]), or *response-request I/O*.

In Haskell, the value of the program has type `Dialogue`, a synonym for a function between a list of I/O responses to a list of I/O requests:

```hs
type Dialogue = [Response] -> [Request]
main :: Dialogue
```

`Request` and `Response` are algebraic data types which embody all the possible I/O operations and their results, respectively:

```hs
data Request = Putc Char | Getc
data Response = OK | OKCh Char
```

(For the purposes of exposition we have grossly simplified these data types compared with those in standard Haskell).

A system "wrapper program" repeatedly gets the next request from the list of requests returned by main, interprets and performs it, and attaches the response to the end of the response list to which main is applied.

Here, for example, is the `echo` program written using a Dialogue:

```hs
echo :: Dialogue
echo resps = Getc :
  if (a == EOF)
  then []
  else Putc a : echo (drop 2 resps)
  where
  OKCh a = resps !! 1
```

The difficulties with this programming style are all too obvious, and have been well rehearsed elsewhere (Perry [1991]):

* It is easy to extract the wrong element of the responses, a *synchronization error*. This may show up in a variety of ways. If the 2 in the above program was erroneously written as 1, the program would fail with a pattern-mathing error in `getCharIO`; if it were written 3 it would *deadlock*.

* The `Response` data type has to contain a constructor for every possible response to every request. Even though `Putc` may only ever return a response `OKChar`, the pattern-matching performed by `Getc` has to take account of all these other responses.

* Even more seriously, the style is not composable: there is no direct way to take two values of type `Dialogue` and combine them to make a larger value of type `Dialogue`.

Dialogues and the IO monad have equal expressive power by using Dialogues to emulate the IO monad, and vice versa. The function `dToIO`, which emulates Dialogues in terms of `IO` is rather curious, because it involves applying the single dialogue `d` to both bottom (⟘) and (later) to the "real" list of responses (Hudak & Sundaresh [1989]; Peyton Jones [1988]). This causes both duplicated work and a space leak, but no more efficient purely-functional emulation is known. The reverse function, `ioToD` does not suffer from these problems, and this asymmetry is the main reason that Dialogues are speciffied as primitive in Haskell.

```hs
-- Dialogue to IO
dToIO :: Dialogue -> IO ()
dToIO d = case (d bottom) of
  []   -> doneIO
  q:qs -> doReq q `bindIO` \r -> dToIO (\rs -> tail (d (r:rs)))

bottom :: a
bottom = error "Should never be evaluated"

doReq :: Request -> IO Response
doReq (GetChar f)   = getCharIO f `bindIO` (\c -> unitIO (OKChar c))
doReq (PutChar f c) = putCharIO f c `seqIO` unitIO OK


-- IO to Dialogue
type IO a = [Response] -> (a, [Request], [Response])

ioToD :: IO () -> Dialogue
ioToD action = \rs -> case action rs of
  (_, qs, _) -> qs

unitIO v = \rs -> (v, [], rs)

bindIO op fop = \rs ->
  let (v1, qs1, rs1) = op rs
      (v2, qs2, rs2) = fop v1 rs1
  in  (v2, qs1 ++ qs2, rs2)
```
