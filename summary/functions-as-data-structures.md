# Functions-as-data-structures

Function as a structure (FaaS), i.e. using functions to store data.

- function as a graph, function as a computation
- function as a lookup table, finite map
- memoization


## Functions as dynamic maps

Like other relations, functions are sets of ordered pairs. Given an input (element of the domain A), a function computes the output (element of the codomain B). Each distinct input is associated with an output, comprising an (input,output) or a (key,value) pair. The set of all these order pairs is called the *graph of a function*.

A graph of a function can also be seen as a mapping form inputs (keys) to outputs (values), a *dynamic map*.

This map could potentialy be infinite, depending on the finitness of the domain. In CS, types correspond to sets, and even though types are necessarily finite, being restricted by the amount of available memory, some are practically infinite, including `String` and `Integer`, as well as all their derivations.

Thus, a function `a -> b` is a, possibly infinite, dynamic map that maps the input of type `a` to the output of type `b`. It is dynamic because given an input value (a key), it must perform some amount of computation before returning the corresponding output (value component in that key-value pair).

## Functions as containers

A partially applied function can be regarded as a simple container. In Haskell, all functions are potential closures, and a closure can hold onto a value, but it can also capture a part of the environment, which endows it with some structural properties and further blurs the distinction between a "proper" data structure and a function imposing as one.

To store `n` values, we need a `n+1`-ary function. The type of the extra (last) parameter is irrelevant since it only acts as a trigger - the function accepts aguments until it (being the last parameter) is filled, it may as well be `()`. A very interesting property of function-as-a-collection (FaaC) is it's heterogeneity, since each argument may have a distinct type. Out of the box, only tuples offer the same feature (and a different API).

The simplest container is the binary function `const` that can hold a single value.

```hs
store1 = const 42
get1 = store1 ()

pair :: a -> b -> (a -> b -> Either a b) -> Either a b
pair x y s = s x y
```





## Finite maps

Functions as static maps

A function used like this is sometimes called *finite map*. Its distinguishing feature is the lack of computation - given an input, rather than calculating the output, it performs a lookup instead. For example, a function can be set up in advance and then used as a lookup table. You call such a function by passing it a value as a "key"; the function will then return the *value* that corresponds to that key, if it exists. Otherwise, you may set it to return some default value, insted of just issuing an error; it may even prompt you to pass it a value, so it inserts a new k/v entry.


## Memoization

This is similar to the technic of function *memoization*, where a function just retrives the approapriate output, given an input that it has seen before; in case the input is unseen, the function performs the computation and returns the output, but not before it stores that input/output pair as a new key/value entry into its memo table. Memoization helps save computational resources by employing a caching mechanism.
