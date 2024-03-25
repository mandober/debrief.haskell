# Lazy evaluation strategy

## Topics
- normal order evaluation
- non-strict evaluation
- advantages of laziness
- infinite data structures
- construct carelessly, consume as necessary
- pay only for what is consumed
- build as you consume
- tying the knot
- knot-tying semantics
- levels of laziness (data vs newtype)
- thunks
- evaluation: strict vs nonstrict
- evaluation: eager vs lazy
- 'call-by' strategies

## Semantics

The terms "evaluation strategy" and "reduction strategy" are often used interchangibly and it is unclear to which category of semantics the terms "lazy", "eager", "strict", "non-strict", "calling convention", "call-by-value", "call-by-name", "call-by-need", etc. exactly belong to - they are part of semantics though, that much is clear.

## Evaluation strategies

Most PLs are eager and strict. *Eagerness* implies that the evaluation of expressions happens immediately (no deferring). This is observed most clearly when considering the evaluation of arguments to a function - first, all the args are evaluated, in the left to right order, and only then are they passed into a function. This behavior is called *call-by-value*, and most imperative languages implement it. A related "calling convention" is *call-by-reference*, where an arg is also passed into a function by value, but since its direct value (on the stack) is a reference (a pointer to data on the heap) - which happens e.g. if the value is larger than a word - it only seems as if the arg is passed in a different way (i.e. by ref) because its value is actually a reference (pointer). Some languages leave the size of a value to determine whether it is passed by value-as-payload or value-as-reference-to-payload, and some pass all values by reference.

Eagerness is contrasted with *laziness*. With lazy evaluation, the args are passed into a function unevaluated. The body of the function will then determine how and when the args will be evaluated. This is actually called *call-by-need* and lazy evaluation is a more general aspect of this behavor, implying that expressions are only evaluated if absolutely needed.

*Strictness* usually means that the evaluation order is well-defined, i.e. there is a precisely defined strategy how a block of code is evaluated, and it is usually done in a top-down manner, statement by statement. Importantly, shuffling the statements around is not permitted unless the compiler can prove that doing that would not affect the language semantics. In imperative languages, this is very hard to prove because any inoucuous statement may have side effects, and effectfull statements must not be executed out of order.

*Non-strict* semantics implies that the compiler has more of a leeway in deciding the evaluation order and possible shuffling of statements. Non-strictness imples a less precisely defined evaluation order compared to strictness.

These 4 evaluation strategies usually form two orthogonal dimensions, with one axis plottiing the eager vs lazy, and the other strict vs non-strict semnatics. However, orthogonality here is very questionable because there are no languages that are strict and lazy, or non-strict and eager - it all comes down to the two categories: *eager-and-strict* vs *lazy-and-nonstrict*.

```
j  s         strict
c              │
               │
               │
eager ─────────┼───────── lazy
               │
               │      h
               │
          non-strict
```

Different levels of conformity to the chosen eval strategy are possible though. For example, Java is positioned is the dead top left corner. C is as eager as Java but is less strict. On the other hand, Scala is as strict as Java but less eager, since Scala offers ways to annotate laziness of variables. In the other extreme is Haskell, being non-strict and lazy, but even Haskell is not that much lazy and non-strict - its position in the diagram is not kissing the max values of laziness and nonstrictness.

## Evaluation in Haskell

Actually, when examining these aspects in Haskell, we usually talk about a single dimension which extremes are labeled strict and lazy.

```
strict ←─────────┼─────────→ lazy
```

Haskell is famous for being a lazy language, which comes off as an intriguing property in a world of strict (and eager) languages. There are many functional programming languages which are not lazy, however, they are also not pure. In fact, there is an argument that purity requires laziness (which is "out of scope" of this article because I forgot how the two are exactly related).

Haskell is lazy "by default". This property allows us to write code carelessly, without considering the efficiency, trusting that GHC will correctly decide which pieces of the code need to be made strict and how to pull off the evaluation strategy. Being lazy actually means letting the compiler make most decisions by analyzing our code. GHC performs several code analysis deciding for which expressions the evaluation should be delayed, suspended, and which expressions should be evaluated eagrly. Among the compilers, GHC has the most wiggle room. It has the most freedom when it comes to evaluation and reordering of expressions. It can do this because of the purity, i.e. there are no side effects lurking all over the place. Instead, effectfull computations are clearly delimited under the purview of the `IO` monad. GHC performs the code analysis in terms of the *demand*; roughly, when does one expression require the results of another.

## Benefits of laziness

All PLs have some aspects of laziness, except these aspects are tighlty controlled by the compiler; that is, all the places where a little laziness is beneficial are known. This is called *short-circuiting* and includes, for example, logical conjunction and disjunction. Some languages go further, having compiler pragmas or language constructs used to annotate the parts of code that the programmer wants to be lazily evaluated.

In Haskell, things are lazy by default so instead, there are language constructs (sigils like `!` and `~`, functions like `seq`, compiler pragmas like `StrictData`) used by the programmer to explicitly anotate the evaluation strategy in cases when the programmer knows better than the compiler.


### Creating infinite data structures

means you don't have to calculate in advance how many elements of some collection you're gonna need - you can just construct an infinite collection which wil be evaluated only as much as needed.

### Cyclic structures

In Haskell, creating a cyclic structure, such as a doubly-linked list, seems impossible. However, laziness allows for such definitions, and we can exploit that using the procedure called "tying the knot".

The simplest example:

```hs
cyclic = let x = 0 : y
             y = 1 : x
         in  x
```

This creates a cyclic list consisting of the repeated sequences of 0 and 1. Importantly, this allocates only two numbers (0 and 1) in memory, making this a truly cyclic list.

The knot analogy stems from the fact that we produce two open-ended objects, and then we link their ends together.

Evaluation of the above looks something like:

```hs
  cyclic
= x
= 0 : y
= 0 : 1 : x -- Knot! Back to the beginning.
= 0 : 1 : 0 : y
= -- etc.
```

Due to Haskell's laziness, while we're building the node, we can set its children to the final value right then and there, even though we don't yet know what that final values are!





## Refs

Laziness in Haskell - with Alexis King
Strictness, laziness, demand, demand analysis, thunks.
https://www.youtube.com/playlist?list=PLyzwHTVJlRc8620PjqbM0x435-6-Gi1Gu
