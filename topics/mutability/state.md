# State

* An updatable state can be modelled in a purely FPL by passing each function the current contents of the state as an additional parameter, and returning the possibly modied state as a part of each function's result.


```
s₀ (intial state)
↓
f¹ --> (r¹, s¹)
            ↓
(r², s²) <- f²
     ↓
     f³ -> (r³, s³)
                ↓
(r⁴, s⁴) <----- f⁴
================== return results
(r¹, r², r³, r⁴)
```


## State

http://learnyouahaskell.com/for-a-few-monads-more#state

Purity is dandy, but some problems are inherently stateful, relying on the state that is changing over time. How can we model a state, that is, a possibly global implicit environment (a set of vars or a key/value map) in a pure functional setting that requires returning the same output given the same input lest break referential transparency.

We have to thread the state through functions explicitly: stateful functions must take in the state `s`, returning the result `a` along with the modified state, `(a, s)`. The distinctive part of the signature of state-dealing functions will be the segment `s -> (a, s)`.


```hs
-- a stateful function takes the state s,
-- returning the result and the modified state
σ :: s -> (a, s)
σ s = (x, s')

-- state type useless shorthand
type State s a = :: s -> (a, s)

-- state type usefull shorthand
newtype State s a = State { runState :: s -> (a, s) }

-- threading the state
stfu state0 = let (a, state1) = calculation state0
                  (b, state2) = calculation state1
              in  (a + b, state2)

-- takes an initial state (discards it at the end) and some other param
stfu st0 x0 = let (x1, st1) = calc x0 st0
                  (x2, st2) = calc x1 st1
              in  x2
```

The state monad makes dealing with stateful problems easier by hiding the state and the threading of the state.



## Hauling state

When dealing with random numbers, we had functions that accept a random generator as an arg and returned a pair made of a random number and a new random generator. If we wanted to generate several random numbers, we had to use the random generator that the previous function returned. When making a function that takes a `StdGen` and tosses a coin three times based on that generator, we had to do this:

```hs
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen0 =
  let (coin1, gen1) = random gen0
      (coin2, gen2) = random gen1
      (coin3, gen3) = random gen2
  in  (coin1, coin2, coin3)

r1 = threeCoins $ mkStdGen 1 -- (True,True,False)
r2 = threeCoins $ mkStdGen 1 -- (True,True,False)
r3 = threeCoins $ mkStdGen 2 -- (True,False,True)
```

It took a generator `gen` and then `random gen#` and returned a `Bool` along with the new generator. To throw the second coin, we used the new generator, and so on.


## Imperative state

In imperative languages, we wouldn't have to return a new generator along with a random number, we could just modify the existing one. But since Haskell is pure, we can't do that, we have to haul state around.

Assignment in imperative languages could be modelled as a stateful computation, depending on whether the assignment is an expression or statement. In a language where the assignment is an expression, `x = 5` will assign the value `5` to the variable `x` but the overall value of this expression will also be the value `5`.

Functionally, we could say that a procedure takes a state (e.g. an implicit global env that maps var names to their current value), returns the result (e.g. 5), along with the new state (the previous mappings extended with the new entry, with "x" as the key and 5 as its value).



This implicit global env is usually called the symbol table and it is a key/value store. The keys are var names (so `String`s) and the values are the var's values so they can have any type (`a`). The symtable could be repr as a `[(String, a)]` but the problem is that values are heterogeneous. Therefore it may be better repr as `Map String Ty` where `Ty` is a sum type representing all the base types of the imperative language.

key :: String | value :: Ty
--------------|----------------------
x             | 5       :: Ty Int
is_checked    | true    :: Ty Bool
name          | "abcde" :: Ty String


## Back to state

To understand the concept of stateful computations, we'll give them a type that says that a stateful computation is a function that takes some state and returns a value and a new state. That function could have the type `s -> (a,s)` where `s` repr the type of the state, and `a` repr the result of a stateful computation.

A stateful computation, that is, a function that takes a state and returns a result and a new state, can also be thought of as a *value with a context*. The actual value is the result (`a`), whereas the context reflects in the fact that we must provide some initial state (`s`) to actually get the result; and in the fact, that along with the result, we get back the new state as well, `(a, s')`.

This is also a kind of an embellishment; we embellish a function `a -> b` with the context, so it becomes `a -> (t, a)`. Another kind of embellishment produces the function `a -> m b`, and yet another `a -> [b]`. These embellishments allow us to model computations that might fail, stateful computations, non-deterministic computations, etc.

## Stateful stack

As an example of a stateful computations, we'll model the stack data structure using a list, and it head will repr the element at the top of the stack.

Unlike in imperative languages, here all stack ops must pass the stack around, with, possibly, more values. In fact, all functions could be typed as taking a pair and returning a pair, `(s, v) -> (s, v)`, where `s` repr the stack, and `v` repr a value whose type could be the type of stack's elements or the unit type.

```hs
-- possible representations

(Stack a, b) -> (Stack a, b) ≅
  where b ~ a | b ~ ()
(b, Stack a) -> (Stack a, b) ≅
b -> Stack a -> (Stack a, b) ≅
```

For example, the `pop` function takes a stack and returns the modified stack and the popped element, while the `push` function takes a value and a stack and returns the new stack with that value on top.

```hs
newtype Stack a = Stack [a] deriving Show

data StackPair a = StackVal (Stack a, a) | StackUnit (Stack a, ())

pop  :: () -> Stack a -> (Stack a,  a)
push ::  a -> Stack a -> (Stack a, ())
```
