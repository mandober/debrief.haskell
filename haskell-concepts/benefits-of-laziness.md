# Benefits of laziness

- normal order evaluation
- non-strict evaluation
- advantages of laziness
- infinite data structures
- construct carelessly, consume as necessary
- pay only for what's consumed
- build as you consume
- tying the knot
- knot-tying semantics
- levels of laziness (data vs newtype)
- thunks
- Evaluation: strict vs nonstrict
- Evaluation: eager vs lazy
- Various 'call-by' strategies


## Creating infinite data structures

means you don't have to calculate in advance how many elements of some collection you're gonna need - you can just construct an infinite collection which wil be evaluated only as much as needed.

## Cyclic structures

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
