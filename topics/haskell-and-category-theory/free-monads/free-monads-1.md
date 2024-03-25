# Free monads

John DeGoes: `Beyond Free Monads`, λC Winter Retreat, 2017
LambdaConf 2017 - Winter Retreat
https://www.youtube.com/watch?v=A-lmrvsUi2Y

## Intro

Free monads and free applicatives have proven an incredibly useful tool in repertoire of the functional programmer: they separate concerns, encourage denotational semantics for program specification, allow easy and type-safe mocking of purely functional code, and allow dynamic introspection and optimization.

Despite these benefits, free monads are notoriously constrained: by themselves, they cannot handle parallelism (only sequentiality), and because they provide only a monad, richer structures (such as monads that fail, or monads that support alternation) cannot be expressed without crude hacks that limit composability and expressiveness.

In this session, John A. De Goes shows how the free monad can be deconstructed for its individual features, and then rebuilt using a more powerful technique that enables more extensibility. The resulting structure - no longer technically a "free monad" - allows reification of as few or as many aspects of computation as are necessary to model the problem domain.

After the session, attendees will know how to augment their existing free programs to add parallelism, racing, failure, and other aspects of computation as required by their problem. In addition, through this thorough deconstruction and reconstruction of the free monad, attendees will have a very deep understanding of reified computation and why the free monad has the structure and limitations it does.

## Reinventing Free: pure effects

Instead of doing effects directly, we can define a data structure that describes the intended effects.

```hs
data ConsoleIO
  = WriteLine String ConsoleIO
  | ReadLine (String -> ConsoleIO)
  | End
```

`ConsoleIO` represents the rest of the program after we have read the line from the console. `ReadLine` takes a function that expects a string read from the console. When we give it a string (line of input read from the console), it gives us back the rest of the program. `End` data ctors enables us to stop the program, it marks a program's end.

The problem with this data structure is that programs expressed this way are not composable. This is the direct consequence of `End` not returning any value. To fix this, we introduce a type param `a` and thread it through all ctors:

```hs
data ConsoleIO a
  = WriteLine String (ConsoleIO a)
  | ReadLine (String -> ConsoleIO a)
  | EndWith a
```

The `End` data ctor becomes `EndWith a`, where the `a` is the value we end our program with. Now `ConsoleIO a` is a console program that returns a value of `a` type.

If we have a program that returns an Int, perhaps we want to turn that program into the one that produces a String in an easy way. This is most easily done by introducing the map operation. Basically, `ConsoleIO` is a functor, although the given type signature for `Map` is kinda weird… why not the good old fmappy signature like `fmap :: (a -> b) -> ConsoleIO a -> ConsoleIO b`?

```hs
data ConsoleIO a
  = WriteLine String (ConsoleIO a)
  | ReadLine (String -> ConsoleIO a)
  | EndWith a
  | Map (forall z. (forall a0. ConsoleIO a0 -> (a0 -> a) -> z) -> z)
```

## Sequencing operations

We get simpler effect control with the bind (chain) operator that allows us to make data dependencies. We can force an operation B to depend on operation A, so that A must be exectured first and then its result fed to B.

```hs
data ConsoleIO a
  = WriteLine String (ConsoleIO a)
  | ReadLine (String -> ConsoleIO a)
  | EndWith a
  | Map   (forall z. (forall a0.
      ConsoleIO a0 -> (a0 ->           a) -> z) -> z)
  | Chain (forall z. (forall a0.
      ConsoleIO a0 -> (a0 -> ConsoleIO a) -> z) -> z)
```

`Chain` is the `bind` (or `flatMap`) operation represented as a data structure. It is a means to have dependencies between programs.

```hs
data ConsoleIO a
  = WriteLine String a
  | ReadLine (String -> a)
  | Pure a
  | Chain (forall z. (forall a0.
      ConsoleIO a0 -> (a0 -> ConsoleIO a) -> z) -> z)
```
