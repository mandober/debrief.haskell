# Lazy Functional State Threads

`Lazy Functional State Threads`, 1994 
by John Launchbury, Simon L. Peyton Jones

## Abstract

Some algorithms make critical internal use of updatable state, even though their external specification is purely functional. Based on earlier work on monads, we present a way of securely encapsulating stateful computations that manipulate multiple, named, mutable objects in the context of a non-strict, purely-functional language. The security of the encapsulation is assured by the type system, using parametricity. Intriguingly, this parametricity requires the provision of a (single) constant with a rank-2 polymorphic type. 

## 1. Introduction

Purely functional programming languages allow many algorithms to be expressed very concisely, but there are a few algorithms in which *in-place updatable state* seems to play a crucial role. If nothing else, there is, one, absolutely unavoidable, use of state in every functional program: input/output. In many programs these I/O effects are rather complex, involving interleaved reads from and writes to the world state.

We use the term *stateful* to describe computations or algorithms in which the programmer really does want to manipulate (updatable) state.

What has been lacking until now is a clean way of describing such algorithms in a pure, lazy FPL without throwing away the main virtues of FPLs: independence of order of evaluation (the Church-Rosser property), referential transparency, non-strict semantics, and so on.

In this paper we describe a way to express stateful algorithms in Haskell. The
approach is a development of our earlier work on monadic I/O and state encapsulation (Launchbury [1993]; Peyton Jones & Wadler [1993]), but with an important technical innovation: we use *parametric polymorphism* to achieve safe encapsulation of state. It turns out that this allows mutable objects to be named without losing safety, and it also allows I/O to be smoothly integrated with other state mainpulation.

The other important feature of this paper is that it describes a complete system (implemented in GHC) with the following properties:

>Complete referential transparency is maintained.
At first it is not clear what this statement means: how can a stateful computation be said to be referentially transparent? To be more precise, a stateful computation is a *state transformer*, i.e. a function from an initial state to a final state. It is like a "script", detailing the actions to be performed on its input state. Like any other function, it is quite possible to apply a single stateful computation to more than one input state.

So, a state transformer is a pure function. But, because we guarantee that the state is used in a single-threaded way, the final state can be constructed by modifying the input state in-place. This efficient implementation respects the purely-functional semantics of the state-transformer function, so all the usual techniques for reasoning about functional programs continue to work. Similarly, stateful programs can be exposed to the full range of program transformations applied by a compiler, with no special cases or side conditions.

>The programmer has complete control over where inplace updates are used and where they are not.
For example, there is no complex analysis to determine when an array is used in a single-threaded way. Since the viability of the entire program may be predicated on the use of in-place updates, the programmer must be confident in, and be able to reason about, the outcome.

>Mutable objects can be named.
This ability sounds innocuous enough, but once an object can be named its use cannot be controlled as readily. Yet naming is important. For example, it gives us the ability to manipulate multiple mutable objects simultaneously.

>Input/output takes its place as a specialised form of stateful computation.
Indeed, the type of I/O performing computations is an instance of the (more polymorphic) type of stateful computations. Along with I/O comes the ability to call imperative procedures written in other languages.

>It is possible to encapsulate stateful computations
so that they appear to the rest of the program as pure (stateless) functions which are guaranteed by the type system to have no interactions whatever with other computations, whether stateful or otherwise (except via the values of arguments and results, of course). Complete safety is maintained by this encapsulation. A program may contain an arbitrary number of stateful sub-computations, each simultaneously active, without concern that a mutable object from one might be mutated by another.

>Stateful computations can even be performed lazily without losing safety.
For example, suppose that stateful depth-first search of a graph returns a list of vertices in depth-first order. If the consumer of this list only evaluates the first few elements of the list, then only enough of the stateful computation is executed to produce those elements.

## 2. Overview

This section introduces the key ideas of our approach to stateful computation. We begin from the programmer's perspective.

### 2.1 State transformers

A value of type `ST s a` is a computation which transforms a state indexed by type `s`, and delivers a value of type `a`. We can think of it as a box:

```
              ╭──────────────╮
              │              │─────→ result
              │              │
state in ────→│              │─────→ state out
              ╰──────────────╯
```

Note that this is a purely-functional account of state.

The `ST` stands for *a state transformer*, which we take to be synonymous with *stateful computation*: the computation is seen as transforming one state into another (of course, it is our intention that the new state will actually be constructed by modifying the old one in place).

A state transformer is a first-class value. A state transformer can have other inputs besides the state; if so, it will have a functional type. It can also have many results, returning them in a tuple.

For example, a state transformer with two inputs of type Int, and two results of type Int and Bool, would have the type:

```hs
st :: Int -> Int -> ST s (Int,Bool)
```

Its picture might look like this:
```
              ╭──────────────╮
input 1 ─────→│              │─────→ output 1
input 2 ─────→│              │─────→ output 2
              │              │
state in ────→│              │─────→ state out
              ╰──────────────╯
```

The simplest state transformer, `returnST`, simply delivers a value without affecting the state at all:

```hs
returnST :: a -> ST s a
```

The picture for `returnST` looks like this:

```
              ╭──────────────╮
input ───────→│──────────────│─────→ result
              │              │
state in ────→│┄┄┄┄┄┄┄┄┄┄┄┄┄┄│─────→ state out
              ╰──────────────╯
```

### 2.2 References

>What is a state?
A part of every state is a *finite mapping from references to values* 
(a state may also have other components, as we will see in Section 4).

A **reference** can be thought of as the name of (or an address of) a variable, an updatable location in the state capable of holding a value.

The following 3 primitive operations are available:

```hs
newVar     :: forall a s.               a -> ST s (MutVar s a)
readVar    :: forall a s. MutVar s a      -> ST s a
writeVar   :: forall a s. MutVar s a -> a -> ST s ()

-- MutVar is later renamed to STRef and defined in Data.STRef
newSTRef   :: forall a s.              a -> ST s (STRef s a)
readSTRef  :: forall s a. STRef s a      -> ST s a
writeSTRef :: forall s a. STRef s a -> a -> ST s ()
```

The function `newVar` takes an initial value of type `a` and delivers a state transformer of type `ST s (MutVar s a)`. When this is applied to a state, it allocates a fresh reference, i.e. one currently not used in the state. It augments the state with a mapping from this reference to the supplied value, and returns the reference along with the modified state.

The type `MutVar s a` is the type of references allocated from a store of type `s`, containing a value of type `a`. Unlike, e.g. SML's 'Ref' types, `MutVars` are *parameterised over the type of state and the type of value* to which the reference is mapped by the state.

Given a reference `v`, `readVar v` is a state transformer which leaves the state unchanged, but uses the state to map the reference to its value.

The function `writeVar` transforms the state so that it maps the given reference to a new value. Notice that the reference itself does not change; it is the state which is modified. `writeVar` delivers a result of the unit type. A state transformer of type `ST s ()` is useful only for its effect on the state.

### 2.3 Composing state transformers

State transformers can be composed in sequence, to form a larger state transformer, using `thenST`, which has type

```hs
thenST :: ST s a -> (a -> ST s b) -> ST s b
```

The picture for (s1 `thenST` s2) looks like this:

```
          ╭───────────────────────────────────────────╮
          │   ╭──────────────╮      ╭──────────────╮  │              
          │   │              │─────→│              │──┼──→ result   
          │   │      s1      │      │      s2      │  │             
state in ─┼──→│              │─────→│              │──┼──→ state out
          │   ╰──────────────╯      ╰──────────────╯  │              
          ╰───────────────────────────────────────────╯
```

Notice that the two computations must manipulate state indexed by the same type, `s`. Also, `thenST` is inherently sequential, because the state consumed by the second computation is that produced by the first.

We often refer to a state transformer as a *thread*, invoking the picture of a series of primitive stateful operations that are *threaded together* by a state passed from one to the next.

Putting together what we have so far, here is a "procedure" which swaps the contents of two variables:

```hs
swap :: MutVar s a -> MutVar s a -> ST s ()
swap v w = readVar v `thenST` (\a ->
  readVar w `thenST` (\b ->
    writeVar v b `thenST` (\_ ->
      writeVar w a)))

swap :: STRef s a -> STRef s a -> ST s ()
swap mv mw = do
  v <- readSTRef mv
  w <- readSTRef mw
  writeSTRef mv w
  writeSTRef mw v
```

`thenST` is a bind monadic operator since `ST` is a monad.

```hs
return :: a -> ST s a
(>>=)  :: ST s a -> (a -> ST s b) -> ST s b
(>>)   :: ST s a -> ST s b -> ST s b
```

Furthermore, we provide a special form of `thenST`, called `thenST_`, with the following type signature:

```hs
thenST_ :: ST s () -> ST s b -> ST s b
```

Unlike `thenST`, the second argument of `thenST_` is not a function, so the lambda isn't required. So we can rewite `swap` as follows:

```hs
swap :: MutVar s a -> MutVar s a -> ST s ()
swap v w = readVar v `thenST` \a ->
  readVar w `thenST` \b ->
    writeVar v b `thenST_`
      writeVar w a
```

When `swap v w` is executed in a state thread (that is, when given a state), `v` is *dereferenced*, returning a value which is bound to `a`. Similarly the value of `w` is bound to `b`. New values are then written into the state at these locations, these values being `b` and `a` respectively.

In addition to `thenST` and `returnST`, we have found it useful to introduce one other "plumbing" combinator, `fixST`, with the type

```hs
fixST :: (a -> ST s a) -> ST s a
```

and the usual *knot-tying semantics*, which we depict thus

```
        ╭───────────────────────╮
        │     ╭──────────────╮  ↑
        ╰────→┼──────────────┼──┴──→ result
              │      s       │
state in ────→┼┄┄┄┄┄┄┄┄┄┄┄┄┄┄┼─────→ state out
              ╰──────────────╯
```

This is the only point that relies on laziness. Everything else in the paper is directly applicable to strict languages.

### 2.4 Encapsulation

So far we have been able to combine state transformers to make larger state transformers, but how can we make a state transformer part of a larger program which does not manipulate state at all? What we need is a function, `runST`, with a type something like the following:

```hs
runST :: ST s a -> a
```

The idea is that `runST` takes a state transformer as its argument, conjures up an initial empty state, applies the state transformer to it, and returns the result while discarding the final state. The initial state is "empty" in the sense that no references have been allocated in it by `newVar`; it is the empty mapping.

But there seems to be a terrible flaw: what is to prevent a reference from one thread being used in another? For example:

```hs
let v = runST (newVar True)
in  runST (readVar v)
```

The reference allocated in the first `runST`'s thread is used inside the second `runST`. Doing so would be a great mistake, because reads in one thread are not sequenced with respect to writes in the other, and hence the result of the program would depend on the evaluation order used to execute it.

It seems at first that a runtime check might be required to ensure that references are only dereferenced in the thread which allocated them.

Unfortunately this would be expensive. Even worse, our experience suggests that it is surprisingly tricky to implement such a check - the obvious ideas fail as it then becomes possible to test the identity of a thread and thus losing referential transparency - and we still do not know a straightforward way to do so.

This problem brings us to the main technical contribution of the paper: the difficulties with `runST` can all be solved by giving it a more specific type.

The type given for `runST` above is implicitly universally quantified over both `s` and `a`.

Now, what we really want to say is that `runST` should only be applied to a state transformer which uses `newVar` to create any references which are used in that thread. To put it another way, the argument of `runST` should not make any assumptions about what has already been allocated in the initial state. That is, `runST` should work regardless of what initial state it is given.

The type of `runST` should instead be:

```hs
runST :: forall a. (forall s. ST s a) -> a
```

which is not a Hindley-Milner type, because the quantifiers are not all at the top level; it is an example of **rank-2 polymorphism** (McCracken [1984]).

>Why does this type prevent the "capture" of references from one thread into another?

Consider our example again

```hs
let v = runST (newVar True)
in  runST (readVar v)
```

In the last line a reference `v` is used in a stateful thread (`readVar v`), even though the latter is supposedly encapsulated by `runST`. This is where the type checker comes into its own. During typechecking, the type of `readVar v` will depend on the type of `v` so, for example, the type derivation will contain a judgement of the form:

    { …, v : MutVar s Bool } ⊢ readVar v : ST s Bool

Now in order to apply runST we have to be able to generalise the type of  `readVar v` with respect to `s`, but we cannot as `s` is free in the type environment: `readVar v` simply does not have type `forall s. ST s Bool`.

>What about the other way round?

Let's check that the type of runST prevents the "escape" of references from a thread. Consider the definition of `v` above:

```hs
v = runST (newVar True)
```

Here, `v` is a reference that is allocated within the thread, but then released to the outside world. Again, consider what happens during typechecking. The expression `newVar True` has type `ST s (MutVar s Bool)`, which will generalise nicely to `forall s. ST s (MutVar s Bool)`.

However, this still does not match the type of runST. To see this, consider the instance of runST with `a` instantiated to `MutVar s Bool`:

```hs
runST :: (forall s'. ST s' (MutVar s Bool)) -> MutVar s Bool
```

We had to rename the bound variable `s` in the type of runST to avoid it erroneously capturing the `s` in the type `MutVar s Bool`. The argument type now doesn't match `v`'s type. Indeed there is no instance of runST which can be applied to `v`. Just to demonstrate that the type of runST does allow some nice examples here is one that is fine:

```hs
f :: MutVar s a -> MutVar s a
f v = runST (newVar v `thenST` \w -> readVar w)

-- with STRef
f :: a -> a
f v = runST (newSTRef v >>= readSTRef)
f "abc" -- "abc"
```

where `v` is a reference from some arbitrary state thread. Because `v` is not accessed, its state type does not affect the local state type of the short thread (which is in fact totally polymorphic in `v`).

>It is fine for an encapsulated state thread to manipulate references from other threads so long as no attempt is made to dereference them.

In short, by the expedient of giving runST a rank-2 polymorphic type we can enforce the safe encapsulation of state transformers. More details on this are given in Section 5.2, where we show that runST's type can be accommodated with only a minor enhancement to the type checker.

## 3. Array references


## 4. Input/output

Now that we have the state-transformer framework in place, we can give a new account of input/output. An I/O-performing computation is of type `ST RealWorld a`; that is, it is a state transformer transforming a state of type `RealWorld`, and delivering a value of type `a`.

The only thing which makes it special is the type of the state it transforms, an *abstract type* whose values represent the real world. It is convenient to use a type synonym to express this specialization:

```hs
type IO a = ST RealWorld a
```

Since `IO a` is an instance of `ST s a`, it follows that all the state-transformer primitives concerning references and arrays work equally well when mixed with I/O operations. More than that, the same "plumbing" combinators, `thenST`, `returnST` and so on, work for I/O as for other state transformers. In addition, however, we provide a variety of I/O operations that work only on the IO instance of state (that is, they are not polymorphic in the
state), such as:

```hs
putChar :: Char -> IO ()
getChar :: IO Char
```

It is easy to build more sophisticated I/O operations on top of these. For example:

```hs
putString :: String -> IO ()
putString [] = returnST ()
putString (c:cs) = putChar c `thenST_`
putString cs

-- or, equivalently
putString cs = seqST (map putChar cs)
```

There is no way for a caller to tell whether `putString` is "primitive" or "programmed". Indeed, `putChar` and `getChar` are not primitive either.

There is actually only one primitive I/O operation, called `ccall`, which allows the Haskell programmer to call any C procedure.

For example, `putChar` is defined like this:

```hs
putChar :: Char -> IO ()
putChar c = ccall putchar c `thenST` \_ -> returnST ()

-- i.e.
putChar :: Char -> IO ()
putChar c = ccall putchar c >> return ()
```

That is, the state transformer (`putChar c`) transforms the real world by calling the C function `putchar`, passing it the character `c`. The value returned by the call is ignored, as indicated by the wild card. Similarly, `getChar` is implemented like this:

```hs
getChar :: IO Char
getChar = ccall getchar
```

`ccall` is actually implemented as a new language construct, rather than as an ordinary function, because we want it to work regardless of the number and type of its arguments.

The restrictions placed on its use are:
- All the arguments, and the result, must be types which C understands: Int, Float, Double, Bool, Array. There is no automatic conversion of more complex structured types, such as lists or trees.
- The first "argument" of `ccall`, which is the name of the C function to be called, must appear literally. It is really part of the *ccall construct*.


## 5. Formal semantics


## 6. Implementation


## 7. Other useful combinators
