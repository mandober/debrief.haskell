---
downloaded: 2021-09-11
author: Oleg Kiselyov
source: http://okmij.org/ftp/Haskell/polyvariadic.html
page-title: Polyvariadic functions and keyword arguments
page-subtitle: Pattern-matching on the type of the context
page-section-url: http://okmij.org/ftp/Haskell/polyvariadic.html#ZipN
page-section-title: Double-generic zipWith, for any number of any collections
---
# Double-generic zipWith, for any number of any collections

http://okmij.org/ftp/Haskell/polyvariadic.html#ZipN


The function `zipWith` zips 2 lists `[a]` and `[b]` element-wise using a combining function `a -> b -> c` and producing a list `[c]`. To combine 3 lists `zipWith3` is used, for 4 lists `zipWith4`, and so on, up to `zipWith7` (and the same for `zip`, `zip2`..`zip7`).

Writing 7 separate functions that do essentially the same thing is tedious (which is probably why the authors stopped at seven), let alone inelegant.

Incidentally, `map`, which can be called `zipWith1`, is also a member of this family of functions (and `repeat` as `zipWith0`).

We may also want to combine element-wise other collections, such as `Data.Sequence` or arrays, or even zip a list with an array, obtaining either a new array or a new list.

Can we do better then write the multitude of `zipWith` functions for each of these collections, and for each combination of collections?

We describe an alternative - a single function (!) to element-wise combine arbitrarily many collections of arbitrary sorts (lists, sequences, etc).

We hence present the double-generic `zipWith` that is generic both in the number of args and in the sort of collections. Different collections may have elements of different types; in certain implementations, different collections may be of different sorts even (some arrays, some lists, some unpacked arrays or sequences).

We don't have to explicitly specify the number of collections to zip. Here are a few examples of `zipN`; first, we sum up 2 or 3 lists:

```hs
test1 :: [Int]
test1 = zipN succ [1,2,3]

test2 :: [Int]
test2 = zipN (+) [1,2,3] [10,11,12]

test21 :: [Int]
test21 = zipN (((+) .) . (+)) [1,2,3] [10,11,12] [100,200,300]
```

Different collections may have different element types, as the next example, of selecting elements from two arrays based on the boolean mask, shows. In the example, `mask` is a boolean array with bounds 1 through 10; `arr1` and `arr2` are `Float` arrays with bounds `(-1,7)` and `(2,20)` respectively. The resulting `Float` array has the bounds `(2,7)`:

```hs
a₃ = zipN (+) arr1 arr2 :: Array Int Float
ar = zipN (\x y z -> if x then y else z) mask arr1 a₃ :: Array Int Float
```

The last example also selects from two collections according to the boolean mask. It uses the most general `zipN` implementation and demonstrates that the collections to zip do not have to be of the same sort: the boolean mask is an `Integer`, one collection to select from is a `ByteString` and the other is an ordinary String, that is, `[Char]`. The result is returned as a `ByteString`:

```hs
bstr :: B.ByteString
test51 = zipN B.pack (0x5::Integer) bstr "xyz"
    (With $ \x y z -> if x then y else (fromIntegral . ord $ z))
```

We present 3 implementations of `zipWith`, in the *classical triad of thesis, antithesis and synthesis*. All 3 are usable, differing in the sorts of collections they support and if all collections to zip must be of the same sort. They also greatly differ in efficiency: the last, synthesis implementation is most efficient, creating no intermediate collections, filling in the result as it steps through the argument collections in parallel.

We start with the most obvious, and the least efficient implementation. Recall, our goal is to generalize the family

```hs
map      :: (a -> z) ->           [a]               -> [z]
zipWith  :: (a -> b -> z) ->      [a] -> [b]        -> [z]
zipWith3 :: (a -> b -> c -> z) -> [a] -> [b] -> [c] -> [z]
-- etc.

-- replacing it with the single function
zipN :: (a₁ -> a₂ -> a₃ -> … -> aₙ) -> c a₁ -> c a₂ -> c a₃ -> … -> c aₙ
```

## Impl v1

Here, all collections to zip must be of the same sort `c`, which has to be pairwise zippable. That is, it should be possible to zip a pair of collections:

```hs
class Zip2 c where
  zipW2 :: (a -> b -> r) -> c a -> c b -> c r
```

Clearly, lists are pairwise zippable, and so are arrays, for example.

The idea of the first impl comes from the 
*similarity between zipping and the Applicative Functor application*: 
`zipN f a₁ a₂ ... aₙ` is essentially 
`f <$> a₁ <*> a₂ ... <*> aₙ`

assuming the collection type implements the Applicative interface, or at least the `(<*>)` operation. Anything that implements `Zip2` interface is such an *unpointed Applicative*, `(<*>) = zipW2 ($)`.

The problem hence reduces to writing a function that takes an arbitrary number of args and *inserts (<*>) between them*.

One solution is to create a class that **pattern-matches on the return type**, i.e. **on the type of the continuation**, as in

```hs
class ZipN ca r where
  zip_n :: ca -> r

-- The return type is (c r) so produce the result
instance (a ~ r) => ZipN (c a) (c r) where
  zip_n = id

-- The return type is (c a' -> r) so we're given a new collection.
-- We combine it with the accumulator using `<*>`
instance (Zip2 c, a ~ a', ZipN (c b) r) =>
        ZipN (c (a -> b)) (c a' -> r) where
    zip_n cf ca = zip_n (zipW2 ($) cf ca)

-- Finally, our desired zipN
zipN f c1 = zip_n (fmap f c1)
```

This was the entire implementation. The just defined `zipN` can be used as:

```hs
mask    :: Array Int Bool
a₁, a₂  :: Array Int Float

-- element-wise addition
a₃ = zipN (+) a₁ a₂ :: Array Int Float

-- selection according to the mask
ar = zipN (\x y z -> if x then y else z) mask a₁ a₃ :: Array Int Float
```

> In this version of `zipN`, all collections to zip must be of the same sort (e.g. all lists) and implement `Zip2` and `Functor`.

Signatures are mandatory since we need to let `zipN` know when the args stop coming. Which is a drawback. The implementation breaks down for collections that are functions: the instances of `ZipN` become overlapping.

A more serious problem is allocating large amounts of working memory. 
For example, zipping of 3 collections proceeds as

```hs
-- zipping of 3 collections…
x = zipN (f :: t1 -> t2 -> t3 -> r)
         (a₁ :: c t1)
         (a₂ :: c t2)
         (a₃ :: c t3)

-- …proceeds as:
let w1 = fmap f a₁ :: c (t2 -> t3 -> r)
    w2 = w1 <*> a₂ :: c (t3 -> r)
    w3 = w2 <*> a₃ :: c r
in  w3
```

and so produces two intermediate collections (of closures) `w2` and `w3`, becoming garbage at the end. (The implementation hence cannot work with unboxed collections that may not have closures.)

As the antithesis, we negate some design decisions and improve the type inference, avoiding the cumbersome type annotations.

We make the combining function the last arg of `zipN`, and wrap it in the unique `newtype With`. The type of `zipN` now has the following general shape

```hs
zipN ::    c a₁ -> c a₂ -> … -> c aₙ
     -> (With (a₁ -> a₂ -> … -> aₙ -> r))
     -> c r
```

`With` does not let us confuse the combining function with an argument collection, even for collections that are functions.

We can reliably tell the end of the argument list without needing type annotations. With the combining function now coming at the end, the earlier trick of partially applying (zipping) it through argument collections no longer works.

We use a different trick, of building pairs alongside the corresponding currying function. That is, `zipN` now proceeds as

```hs
-- zipping collections…
zipN (a₁ :: c t1)
     (a₂ :: c t2)
     (a₃ :: c t3)
     (With (f :: t1 -> t2 -> t3 -> r))

-- …now proceeds as
let w1   = a₁ :: c t1
    cnv1 = id :: forall u. (t1 -> u) -> (t1 -> u)
    w2   = zipW2 (,) w1 a₂ :: c (t1,t2)
    cnv2 = \g -> \ (a₁,a₂) -> cnv1 g a₁ a₂
        :: forall u. ((t1 -> t2 -> u) -> ((t1,t2) -> u)
    w3   = zipW2 (,) w2 a₃ :: c ((t1,t2),t3)
    cnv3 = \g -> \ (a₁₂, a₃) -> cnv2 g a₁₂ a₃
        :: forall u. ((t1 -> t2 -> t3 -> u) -> (((t1,t2),t3) -> u)
in  fmap (cnv3 f) w3
```


The accompanying source code shows the type classes that implement this stepwise curring/pairing for the arbitrary number of arguments. (The reader is encouraged to try writing those typeclasses as an exercise.) The previous example now looks as:

```hs
a₃  = zipN arr1 arr2 (With (+))
ar  = zipN mask arr1 a₃ (With (\\x y z -> if x then y else z))
```

The annoying type signatures are gone: all the types are inferred. The argument collections still have to be of the same sort and be zippable -- but they can now be functions. We are building collections of tuples rather than of closures, which is marginally better: tuples take less space. Nevertheless, we are still building the intermediate collections `w2` and `w3`, wasting time and space. There has to be a better way.

As synthesis, we look back to the starting point, the standard library function `zipWith`. Thanks to lazy lists, it traverses its two argument lists in parallel: it takes an element from each list, combines them into the result element, and only then looks further through the argument lists. Thus zipping up multiple lazy lists avoids building intermediate lists thanks to laziness. To handle other collections just as efficiently, we have to *lazily* convert them to lazy lists. In other words, the proper zipping should deal not with the collections themselves but with their *views*, as lazy element streams. Following this insight, we define the interface for a streaming view of a collection:

```hs
class Streamable c where
  type StreamEl c :: \*
  toStream :: c -> [StreamEl c]
```

All `Foldable` collections are `Streamable`; other collections, such as bits and `ByteString` are `Streamable` too. The view removes the restriction that all collections to zip must be of the same sort. They can be anything, so long as they are `Streamable`. The result of zipping is now a stream, which has to be converted to a suitable collection. We pass that converter as the first argument to the new `zipN`, which otherwise has the interface of the second implementation. Our running example `zipN (tr :: [r] -> c r) (a₁::c1 t1) (a₂::c2 t2) (a₃:: c3 t3) (With (f::t1->t2->t3->r))` now proceeds as

```hs
let w1   = toStream a₁ :: [t1]
    cnv1 = id
        :: forall u. (t1->u) -> (t1->u)
    w2   = zip w1 (toStream a₂) :: [(t1,t2)]
    cnv2 = \\g -> \\ (a₁,a₂) -> cnv1 g a₁ a₂
        :: forall u. ((t1 -> t2 -> u) -> ((t1,t2) -> u)
    w3   = zip w2 (toStream a₃) :: [((t1,t2),t3)]
    cnv3 = \\g -> \\ (a₁2,a₃) -> cnv2 g a₁2 a₃
        :: forall u. ((t1 -> t2 -> t3 -> u) -> (((t1,t2),t3) -> u)
in  tr $ map (cnv3 f) w3
```

The changes to the second `zipN` implementation are minor.

We have described the double-generic `zipN` that efficiently combines arbitrarily many collections of arbitrary types. The key ideas are pattern-matching on the result type (the type of the continuation); defining the interface of `zipN` to make this pattern-matching unambiguous; operating on the streaming view of collections rather than the collections themselves.

*Exercises* How to verify that the efficient `zipN` really traverses its arguments in parallel and does not build large intermediate collections? How to avoid relying on laziness of Haskell strings? We could then deal with effectful collections such as files and mutable arrays, and also be sure of the evaluation order. What are the drawbacks of `Foldable`? How to zip arrays and finite maps, like `Data.Map`, that is, collections with indexed (keyed) elements?
