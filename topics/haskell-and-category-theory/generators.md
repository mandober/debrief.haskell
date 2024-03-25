# Generators

https://okmij.org/ftp/continuations/generators.html

## Generators: the API for traversal and non-determinism

Generators are popular because they give the benefit of lazy evaluation (modular, on-demand processing) in strict languages, or when lazy evaluation doesn't apply because the computation has observable effects such as I/O.

Computations that can yield control decompose into exceptions and non-determinism. A monad of generators is an exception-monad, i.e. the `LogicT` monad transformer.

The oneliner Haskell definition of `yield` should clarify

```hs
yield x = raise x `mplus` return ()
```

yielding the value `x` means raising an exception on `x` or continuing.

Generators originated as a way to encapsulate the traversal of real or virtual collections; a non-deterministic computation being an example of the latter.

We focus on the convenience of defining generators for new collections and on building generators by composition. Yield then arises as a mode of composition.

Iterators like the for-loop are programmed, rather than builtin.

We show how to program
- not only customary traversals but also accumulating ones
- not only generator statements but also value-producing generator expressions
- not only nesting of generators but also running them side-by-side

## Origin of generators

The modern generators first appeared in the language `Alphard`, developed in the group of Mary Shaw at CMU in mid-1970s, and were inspired by the "generators" in IPL-V and mapping functions in Lisp. In 1975, generators have migrated to the language `CLU` under the name *iterators*.

Both Alphard and CLU stressed data abstraction. Alphard in particular was aimed at the development of verifiably reliable software. Abstraction was indispensable in making (Hoare-style) proofs simpler and modular.

Since iterative computations invariably loop over the elements of some collection, it made sense to separate operations on the current element (the loop body) from obtaining the current element (the loop control).

The goal was to hide the details of the collection, encapsulate the state of the enumeration, and make iterative computations modular and easier to prove terminating and correct. As Shaw et al. wrote, *generators abstract over control as functions abstract over operations*.

About two years later, generators appeared in the programming language `Icon` as the fundamental control structure. Icon, like its predecessor `SL5`, focused on goal-directed evaluation, expressing non-deterministic computations. A generator in Icon is an expression that may produce several values on demand. The expression's context determines the number of values to produce.

Again, there was a separation between obtaining (candidate) values and consuming the values, possibly demanding more. A non-deterministic operation hides the details of generating the candidates and encapsulates the generation state. It represents the traversal of a virtual collection whose elements are computed during the enumeration as needed.

However different Alphard and Icon were, they both had for-loops, implemented in terms of generators that produced integers from `i` to `j` on demand, called `upto(i,j)` in Alphard, and `i to j` in Icon.

## Simple generators and lazy lists

We introduce generators on several simple examples of search and enumeration, and relate them to lazy lists, in particular, Haskell lists.

Generators in Alphard and CLU encapsulate the enumeration of a collection. They have a simple interface with the methods to
- get the current element
- advance the enumeration
- test for the end of enumeration

This is the interface of streams or lazy lists, i.e. lists with cons-cells and elements produced on demand.

At a glance, generators in Icon seem different. The results that a generator produces depend on context. When a single result is sufficient, the first encountered result is produced, e.g.

    sentence := "Store it in the neighboring harbor"
    i := find("or", sentence)

which assigns the value 3 to `i`. If the result produced by a generator doesn't make the enclosing expression succeed, the generator is resumed to produce another value.

    if (i := find("or", sentence)) > 5 then write(i)

Like previously, the first result produced by the generator, 3, is assigned to `i` but this value is not greater than 5 and the comparison fails. The generator is resumed and it produces another result, 23, which makes the comparison succeed, so 23 is written as the value of `i`.

These simple examples are straightforward to implement with lazy lists. The `findIL` is an analogue of Icon's `find` function: it gets the index of an occurrence of the specified pattern in the given string:

```hs
findIL :: String -> String -> [Int]
findIL pat str = str & tails & findIndices (isPrefixOf pat)
```

Since there may be several or none such occurrences, Icon's find is non-deterministic, and so is our findIL, as seen from its type. We take advantage of the function `tails`, to compute the list of all suffixes of `str`. The function `findIndices` checks each element against the predicate `isPrefixOf pat` and returns the indices of the matching elements, i.e. the positions of all occurrences of `pat`.

The clarity of the algorithm gives confidence in its correctness, but also inspires worry about efficiency. The appeal of lazy lists is the removal of that worry: none of the 'suffix' and 'index' computations are done unless demanded. The demand, in Icon and in Haskell, comes from the context. In the example, the context needs a value to bind to `i`.

```hs
s1 = "Store it in the neighboring harbor"

x1 = do
  case (findIL "or" s1 & filter (> 4)) of
    i : _ -> print i
    []    -> return ()
```

The `case` expression asks for the first element in the list of indices that is greater than 4. The `filter` expression asks for the first index, 2, filters it out (since <= 4); asks for the second one, 22, keeps it (since > 4). Finally, the index that passed the test was bound to `i` and printed. Since no more values are demanded, the search has stopped past the second occurrence of the pattern. The example execute similarly in both Icon and Haskell.

The context of a generator may also ask for all values in generator's collection. In Icon, a generator can be resumed repeatedly to produce all its results by using the `every-do` control structure.

    i := find("or", sentence)
      do write(i)

which writes all the positions at which the needle occurs in the haystack (i.e. 3, 23, 33). Since *generation is inherited like failure*, this expression can be written more concisely by omitting the optional `do` clause:

    every write(find("or", sentence))

A lazy list can also produce all elements, in the context of an operation that fully traverses the list and prints its elements. The similarity of the Haskell and Icon code is striking.

```hs
mapM_ print $ findIL "or" sentence
```

The similarity between Haskell's lazy lists and Icon's generators holds only for simple examples. Better abstractions are needed to chain and combine the generators. Haskell's lazy lists become less suitable for generators with a lot of state, and they are of no use at all when the generation has an observable effect, like IO.

## Generators and LogicT

Haskell's lazy lists, although similar to generators in simple examples, fall short of an adequate model. Chaining of generators calls for better abstractions, which become necessary for generators with observable effects. The obvious next candidate model, `MonadsPlus`, is not sufficient either. At the very least, we need `LogicT`.

Icon has several other control structures related to generation, one of which is alternation

    expr1 | expr2

which generates the results of `expr1` followed by the results of `expr2`,

    every write(find("or", sentence1)
              | find("or", sentence2))

writes the positions of "or" in sentence1 followed by the positions of "or" in sentence2, which can be written more compactly using alternation in the second arg of find:

     every write(find("or", sentence1 | sentence2))


Another use of alternation is illustrated by

     (i | j | k) = (0 | 1)

which succeeds if any of `i`, `j`, or `k` has the value 0 or 1.


Writing nested generators like these with Haskell's lists is cumbersome, prompting us to adopt a better abstraction. First, we change the primitive building blocks: instead of the list ctors, we use `mzero` for the empty generator, `return` for the singleton one, `mplus` to model alteration, and `>>=` to model chaining. These are the four operations of `MonadPlus`. Although the operations can be expressed in terms lists - list has a `MonadPlus` instance - we'll maintain the abstraction and treat the operations as primitive.

We translate the last Icon's example by first defining the equivalent of Icon's equality comparison, which fails if the equality doesn't hold, returning `mzero` of the implementor (for lists is `[]`, for Maybe is `Nothing`, for Sum Int is `Sum 0`…).

```hs
equal :: MonadPlus m => m Int -> m Int -> m Int
equal i j = do
  iv <- i
  jv <- j
  if iv == jv then return iv else mzero

tcomp i j k = (i `mplus` j `mplus` k) `equal` (return 0 `mplus` return 1)
```

We could have simplified the notation by writing 0 instead of return 0, taking advantage of overloaded numeric literals in Haskell.

Although `MonadPlus` lets us write `tcomp`, it does not, however, let us use it, for example, in a conditional operator. We cannot implement the conditional, the negation-as-failure, or the collector of all generated values - all the operations that test, query and advance the generator. `MonadPlus` lets us build non-deterministic computations, but not examine them. We need a richer interface, such as `LogicT`.

`LogicT` extends MonadPlus with the operation `msplit` that implements the minimal generator interface, of testing for emptiness, obtaining the current element and advancing.

`LogicT` includes the Icon-style conditional, called `ifte`, written in terms of `msplit`. We can now use our `tcomp`

```hs
ifte :: MonadLogic m => m a -> (a -> m b) -> m b -> m b

tcomp_ex1 :: (LogicT t, Monad m, MonadPlus (t m)) => t m String
tcomp_ex1 = ifte (tcomp (return 2) (return 1) (return 3))
                 (\i -> return $ "Yes: " ++ show i)
                 (return "No")
```

To implement the first Icon example, we need the analogue of Icon's `find`. We 'lift' our earlier implementation in terms of lists to an arbitrary MonadPlus:

```hs
-- The MonadPlus analogue of the lazy cons
mcons :: MonadPlus m => a -> m a -> m a
mcons x xs = return x `mplus` xs

-- foldr replaces nil with mzero and cons with mcons
findIM :: MonadPlus m => String -> String -> m Int
findIM pat str = foldr mcons mzero (findIL pat str)
```

To make the example better, we pretend that sentence1 and sentence2 read their sentences from a file (we only pretend, printing a trace message instead). Our generators now have an *observable effect*, ruling out Haskell lists. Fortunately, `LogicT` has other implementations.

In fact, `LogicT` is a monad transformer, parameterized by the monad responsible for the underlying effects.

```hs
sentence1, sentence2 :: (MonadIO (t IO), LogicT t) => t IO String
sentence1 = liftIO (putStrLn "sentence1") >> return sentence
sentence2 = liftIO (putStrLn "sentence2") >> return "Sort of"

twosen = observe $ iter Nothing $
  (liftIO . print =<< findIM "or" =<< sentence1 `mplus` sentence2)
```

This Haskell code looks quite similar to the Icon code, especially if we view `=<<` as a (call-by-value) function application.

The function `iter` is a variant of `bagofN` of the LogicT library. The latter accumulates all (if the first argument is Nothing) or the first several values produced by a non-deterministic computation, returning the accumulated list. The function `iter` throws away the result since the computations were executed for their side effects. Both `bagofN` and `iter` are written in terms of `msplit`.

```hs
msplit :: MonadLogic m => m a -> m (Maybe (a, m a))


type LogicT :: (Type -> Type) -> Type -> Type
newtype LogicT m a = LogicT {
  unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }


type MonadLogic :: (Type -> Type) -> Constraint
class (Monad m, Alternative m) => MonadLogic m where
  lnot       :: forall a.   m a -> m ()
  once       :: forall a.   m a -> m a
  interleave :: forall a.   m a -> m a -> m a
  msplit     :: forall a.   m a -> m (Maybe (a, m a))
  (>>-)      :: forall a b. m a -> (a -> m b) -> m b
  ifte       :: forall a b. m a -> (a -> m b) -> m b -> m b
  {-# MINIMAL msplit #-}
```

## Suspended computations as generators

This section describes the salient, nowadays, feature of generators - the suspension, or yield. We introduce typical examples and show several unsatisfactory attempts to implement them with the already introduced tools such as `LogicT`.

In the languages with generators, `yield` is a keyword, a special syntactic form with the dedicated support by the run-time system. We would like however to define yield, as a regular library function, in terms of the existing language features. In this section, we try several such features, without full satisfaction. We achieve our goal in the next section.

The generators so far have been built "by hand" (as lists) or by chaining and alternation. Although theoretically sufficient, these methods become ungainly when building generators encapsulating traversals with large amount of state.

In a recursive traversal function, the state is implicit in the local variables of recursive invocations of the function. When writing generators, or zippers, the state has to be explicated, to be manipulated by the three operations of the generator interface (emptiness test, query and advance).

Generators encapsulating traversal are closely related to zippers. *Huet zippers* however deal with algebraic data structures; the algebraic structure helps writing zippers, essentially by differentiation. Generators are more general, encapsulating traversals of virtual collections, with arbitrary effects.

When CLU borrowed generators from Alphard it introduced a new way of building them. Rather than writing the emptiness test, the query and the advance operations and packaging them up, a CLU programmer would use yields.

The only way to write a generator (called iterator) in CLU was by suspending, usually an iterative computation. Since all iterative computations in CLU were built around iterators, yields was a mode of composing them. The inspiration for yields came from Simula 67 coroutines. Icon has a similar form, called suspend, borrowed from SL5, also inspired by Simula's coroutines.

Here is an example of suspend in Icon from the Icon's overview. Applying the procedure `findodd` defined below to the strings `s1` and `s2` non-deterministically gives all odd-valued positions at which `s1` occurs in `s2`.

     procedure findodd(s1, s2)
        every i := find(s1, s2) do
           if i % 2 = 1 then suspend i
     end

Since `find` is itself a generator in Icon, `findodd` is a derived generator (in our case, a "filtered" generator), which gives us an idea of writing findodd with just a `MonadPlus`:

```hs
findodd :: MonadPlus m => String -> String -> m Int
findodd s1 s2 = do
  i <- findIM s1 s2
  if i `mod` 2 == 1
  then return i
  else mzero
```

Alas, the translation from Icon to Haskell is not idiomatic; we would like to reproduce the explicit iteration over find's collection. We accomplish that in the next section.

The construct suspend, often called yield, has become the emblem of generators, spreading to many other languages. Our next example comes from Python, which, since version 2.2, has generators and the keyword `yield` to build them. The example is quoted from David Mertz's Charming Python article, as an elegant, exemplary generator. The similarity to Icon is quite striking.

```py
def inorder(t):
  '''A recursive generator that generates Tree leaves in in-order.'''
  if t:
    for x in inorder(t.left):
      yield x
    yield t.label
    for x in inorder(t.right):
      yield x
```

We describe several attempts at implementing this example in Haskell. 
First, we need a tree datatype

```hs
type Label = Int
data Tree = Leaf | Node Label Tree Tree deriving Show

-- sample complete tree
tree1 = Node 1 (Node 2 (Node 4 Leaf Leaf)
                       (Node 5 Leaf Leaf))
               (Node 3 (Node 6 Leaf Leaf)
                       (Node 7 Leaf Leaf))
```

The first attempt builds the `in_order` generator by alternation. The current element may come from the left branch, from node's label, or right branch:

```hs
inorder1 :: Tree -> Int
inorder1 Leaf = mzero
inorder1 (Node v l r) = inorder1 l `mplus` return v `mplus` inorder1 r

-- We can either collect the labels of all traversed nodes in a list
-- printing it afterwards
inorder1_r = observe (bagofN Nothing $ inorder1 tree1) >>= print
-- [4,2,5,1,6,3,7]

-- or printing the labels as we go
inorder1_r' = observe $ iter Nothing $ inorder1 tree1 >>= liftIO . print
```

This attempt is not satisfactory. Our translation has replaced chaining, implicit in the Python code, by alternation. That trick will not work in general, e.g. when traversing the tree post-order and yielding at each node the sum of the labels, including all descendants. We need the result of a branch traversal, and so we cannot replace chaining (`>>=`) with alternation (`mplus`).

Traversing with yield looks quite similar to the logging of values as we compute, i.e. to the `Writer` monad. It is tantalizing to implement `yield` as `tell`, but it works, in the first approximation - here's traversal generator:

```hs
inorderW :: (MonadWriter [Label] m, MonadIO m) => Tree -> m ()
inorderW Leaf = return ()
inorderW (Node v l r) = do
  inorderW l
  liftIO . putStrLn $ "traversing: " ++ show v
  tell [v]
  inorderW r
```

We have added the statement to print the label (the trace) right before yielding. Alas, this code is too strict, making the traversal not incremental. We have to complete the traversal before we get hold of the accumulated 'log'. The following makes the problem clear:

```hs
inorderW_r = do
  (_labels) <- runWriterT $ in_orderW tree1
  let some_labels = take 3 labels
  print some_labels
```

We only wanted the first 3 labels encountered during the traversal. The printed trace shows that we have traversed the whole tree nevertheless.

Our last attempt uses *final zippers*, implemented with the library of delimited control, the CC monad.

```hs
inorderCC :: Monad m => Tree -> CC (P m Label) m ()
inorderCC Leaf = return ()
inorderCC (Node v l r) = do
  inorderCC l
  yield v
  inorderCC r
```

The code is clear and idiomatic. Yet we hoped to avoid using delimited control explicitly, counting at gaining insight into delimited control and its idioms. We hoped that `LogicT` might turn out sufficient.

## Deriving yield

The earlier unsuccessful attempts inspire us to derive yield by equational reasoning.

The earlier examples have likened yield to raising an exception: both `yield` and `raise` set up a side-channel, for 'side-results' of a computation. Both yield and raise leak values from the depths of an expression. Unlike raise, yield gives a choice to continue the computation. We need non-determinism then. Thus, we need a monad with the operations `raise` and `mplus`. The former should satisfy the law

>raise e >>= k === raise e

formalizing that raising an exception aborts the rest, `k`, of the computation. The operation `mplus` should satisfy the law of left-distributivity:

>(m1 `mplus` m2) >>= k === (m1 >>= k) `mplus` (m2 >>= k)

formalizing that a choice splits the computation.

Monad transformers let us build the monad with the required properties, by applying the exception monad transformer, `ErrorT`, to an instance of `MonadPlus`.

Note: Traditionally, the transformer `ErrorT e` has required the type of the exception `e` to be a member of the `Error` class. Since a generator should be able to yield values of any type, we need an error monad transformer free from this silly restriction. Such a transformer is called `EitherT`.

Since both monads have control effects, the order of the composition matters: applying a `MonadPlus` transformer to an exception monad fails the left distributivity.

Let `e` be a complex computation, which can be written like

>e = m >>= k

or, eta-expanded

>e = m >>= \x -> k x

Let us regard `m` as an 'already computed' part of `e`, with `k` being the remaining part. Since `m` is already computed, it is equivalent to `return v`, where `v` is the (intermediate) result, to be bound to `x` and used in the remainder of `e`. Calculemus:

```hs
ey ≝  m >>= \x -> (raise x `mplus` return ()) >> k x

      { … >> k x === … >>= \ () -> k x }

   =  m >>= \x -> (raise x `mplus` return ()) >>= \() -> k x

      { left distributivity }

   =  m >>= \x -> (raise x   >>= \() -> k x) `mplus` 
                  (return () >>= \() -> k x)

      {the law of exception}

   =  m >>= \x -> raise x   `mplus` 
                  (return () >>= \() -> k x)

      { return is the left unit of bind }

   =  m >>= \x -> raise x `mplus` (k x)

      { recall that m = return v }

   =  (raise v) `mplus` m >>= k
   =  (raise v) `mplus` e
```

Thus, `ey` is a non-deterministic computation that gives us a choice of `e` and of the intermediate result of `e`, up to the point `m`. We have obtained that

```hs
yield :: MonadPlus m => e -> EitherT e m ()
yield x = raise x `mplus` return ()
```

The exception carries the yield's argument 'out of band', on the emergency channel so to speak; the non-deterministic choice `mplus` effectively lets the computation continue despite raising the exception.

We have unwittingly obtained the typing of generators:

`MonadPlus m => EitherT e m a`

is the type of a generator returning the result of type `a` while yielding intermediate results of type `e`.

The derived `yield` lets implement the examples of generators from the previous section - this time, idiomatically.


## Generators with yield in Haskell

We have achieved our goal of defining yield, expressing it in terms of exceptions and alternation. We can implement the examples of building generators by suspension - this time, with satisfaction.

We start with the example of the in-order tree traversal, yielding the node labels encountered along the way. The code below looks as it sounds. We have added the statement to print the trace of the encountered nodes, so we can later tell how far we have gone.

```hs
in_order2 :: (MonadIO m, MonadPlus m) => Tree -> EitherT Label m ()
in_order2 Leaf = return ()
in_order2 (Node v l r) = do
  in_order2 l
  liftIO . putStrLn $ "traversing: " ++ show v
  yield v
  in_order2 r
```

We emphasize that in Python, Ruby, etc., `yield` is a keyword and generators are built-in. In Haskell, yield is a regular, small, user-defined function, and generators are programmed-in.

Recall that a yielded value is transmitted as an exception. *To use the value, we have to catch the exception* - to "run" the `EitherT` monad:

```hs
catchError :: Monad m => EitherT e m e -> m e
catchError (EitherT m) = m >>= check
  where
  check (Left x)  = return x
  check (Right x) = return x
```

A reader familiar with delimited continuations may notice that `catchError` is essentially `reset`. Merging the 'side-channel' into the 'main channel' is the function of the exception handling form, or `reset`.

If the generator only yields, returning no useful result, the following variant of `catchError` is fitting. The code supports the standard, in CLU, Icon and other languages, convention that the normal return from a generator is the end of the traversal.

```hs
catchError' :: MonadPlus m => EitherT e m () -> m e
catchError' (EitherT m) = m >>= check
  where
  check (Left x)  = return x
  check (Right x) = mzero


-- We traverse our sample tree1 completely:
in_order2_r :: IO ()
in_order2_r = observe $ iter Nothing $ do
  i <- catchError' (in_order2 tree1)
  liftIO . putStrLn $ "Generated: " ++ show i


-- or partially, asking only for the first two elements:
in_order2_r' :: IO ()
in_order2_r' = observe $ iter (Just 2) $ do
  i <- catchError' (in_order2 tree1)
  liftIO . putStrLn $ "Generated: " ++ show i
```

The trace confirms the incremental, on-demand traversal. The trace of using a node label is printed right after the trace of encountering the label during the traversal. In the second example, the traversal has stopped right after the desired two elements have been consumed.

We implement the more complex post-order example, yielding at each node the sum of the labels for the subtree rooted at the node. The generator now returns a useful value (the computed sum for all labels in the tree), in addition to yielding the intermediate results. We print the intermediate results, as they are computed, and the final result.

```hs
post_order :: MonadPlus m => Tree -> EitherT Label m Label
post_order Leaf = return 0
post_order (Node label left right) = do
  sum_left  <- post_order left
  sum_right <- post_order right
  let sum = sum_left + sum_right + label
  yield sum
  return sum

post_order_r = observe
             $ iter Nothing
             $ catchError (post_order tree1) >>= liftIO . print
```

We finish with the Icon example of yielding while consuming the results of another generator.

    procedure findodd(s1, s2)
      every i := find(s1, s2) do
        if i % 2 = 1 then suspend i
    end

Our translation now preserves the structure of Icon's code. The function `iterE` below is our old iter 'lifted' to the `EitherT` monad, propagating exceptions.

```hs
findodd2 :: (Monad m, LogicT t, MonadPlus (t m)) =>
            String -> String -> EitherT Int (t m) ()
findodd2 s1 s2 = iterE Nothing $ do
  i <- findIM s1 s2
  if i `mod` 2 == 1 then yield i else return ()
```

## Generator from any Foldable

We have seen that yield turns any traversal to a generator. We confirm this result, literally.

Any abstract data type that implements the `Data.Foldable` interface implements the generator interface. In fact, any (collection) type `T` with the operation like

`mapM_ :: Monad m => (Element T -> m b) -> T -> m ()`

that lets a monadic action examine its elements (of the type `Element T`) implements the generator interface.

The claims holds regardless of the implementation of the data type, whether it is a data structure or if the elements to traverse are computed on-the-fly. The proof of the claim is constructive, short, and trivial:

```hs
foldable_gen :: (MonadPlus m, F.Foldable t) => t a -> EitherT a m ()
foldable_gen = F.mapM_ yield
```

The type says it all: any `Foldable` is a generator. As an example, the following code uses the generator to extract the first 3 elements of a foldable and return them in a list. The traversal is done only to the extent needed to obtain the desired three elements.

```hs
trav :: (Monad m, F.Foldable t) => t a -> m [a]
trav t = observe $ bagofN (Just 3) $ catchError' $ foldable_gen t
```
