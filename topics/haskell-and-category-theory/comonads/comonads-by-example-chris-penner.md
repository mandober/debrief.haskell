# Comonads by Example

`Comonads by Example` - talk by Chris Penner at https://monadic.party    
https://www.youtube.com/watch?v=HOmOQnQGtPU&list=PLcAu_kKy-krxDD1WwRX_9rc0knAFK3nHs&index=7

## TL/DR

This whole comonad business will start to make more sense as soon as you recall that Haskell is lazy, meaning that Haskell builds only the absolutely necessary part of an infinite structure. Mind laziness when attempting to comprehend comonads: don't try to imagine the duplicated structure - especially, e.g. a spreadsheet of spreadsheets - but try to discern why the computation actually needs such slots, what it does with them, why the hell is such an obnoxiously complicated structure needed and good for in the first place?

## Intro

Monads are *effectful computations* - instead of a pure function `a -> b`, monads return the same output but embellished in some context, which is represented by a Kleisli arrow, `a -> m b`.

Monads introduce effects and allow us to compose effects using the Kleisli composition `>=>`, or its variants like `>>=`.

Category theory says we can reverse the arrows and prefix the catogory's name with "co" to get the opposite category, which, in this case, take us from monads to comonads. That way we get the generic comonadic arrow, `w b -> a`.

```hs
kleisli :: Monad m => a -> m b

-- flipping the arrows
cokleisli :: Comonad m => a <- m b

-- normalizing
cokleisli :: Comonad w => w b -> a
```

If monads deal with effects, then comonads deal with *coeffects* and their composition. Coeffects reduce a structure to a single value.

>Coeffects are queries over a structure (aka algebras).

For example, the `length` function over a list is such an operation, and the `head` function. The functions `fst` and `snd` are also queries dependent on the context. BTW, tuple has both Monad and Comonad instances. In fact, a pair is the Writer monad but only if the first component is a Monoid.

>Comonads are structures (contexts) where we run queries.

However, composition of coeffects (queries) presents a slight problem, considering that a comonadic query discards its context, as evident from the type of the cokleisli arrows, `w b -> a`; that is, we end up with just an `a`, without the structure to chain the next query onto.

## Duplicate

As hinted by reversing the arrows of the monadic `join` operation, which flattens a structure, this issue is solved by the opposite operation - by *duplicating the structure*.

```hs
-- monads, a -> m b
join :: m (m a) -> m a

-- comonads, w b -> a
duplicate :: w a -> w (w a)
```

Streams are an examplary comonad, which we can also use to run queries over, like getting an element at an index, droping some elements, etc. In fact, the latter suggests the change from one to another structural contexts, which we'll call a *mutation*, `Stream a -> Stream b`.

>The `duplicate` nests the structure while maintaining context.

The context of a stream is its position. Each node is a stream represents a position, so we're starting off at the zeroth node 'a' and we can see the rest of the stream from there. When we duplicate, we put the entire stream viewable from 'a' onwards into the new node 'a'. At the next slot 'b' we have a stream viewable from 'b' onwards, and at the next slot 'c' we have a stream viewable from 'c' onwards, and so on. `duplicate` is like the list's `tails` function.

```hs
-- original stream
s = a :> b :> c :> …
-- duplicating the stream
duplicate s =
     (a :> b :> c :> …)
  :> (b :> c :> …)
  :> (c :> …)
  :> …
```

> `duplicate` shows us all possible views of a structure.

Each view is in the slot according to some principle. Above, each view is in the slot according to the index. For example, the view of the stream at index 4 is the view of the stream from the 4th element onwards.

The `ix` (index function) focuses a slot within the structure.

```hs
data Stream a = a :> -> Stream a

duplicate :: Stream a -> Stream (Stream a)

ix        :: Int -> Stream a ->        a
drop      :: Int -> Stream a -> Stream a
```

`duplicate` gets us all the views of a structure, and `ix` can select one of those views.

```hs
-- original stream
s = 0 :> 1 :> 2 :> 3 :> 4 :> …

-- index 2 of the original stream
ix 2 s == 2

-- duplicated stream
duplicate s =
     (0 :> 1 :> 2 :> 3 :> 4 :> 5 :> …) -- s₀
  :> (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> …) -- s₁
  :> (2 :> 4 :> 5 :> 6 :> 7 :> 8 :> …) -- s₂
  :> …

-- index 2 of the duplicated stream (s₂)
ix 2 (duplicate s)
  == (2 :> 4 :> 5 :> 6 :> 7 :> 8 :> …) -- ex s₂

-- duplicated stream that was indexed by 2
-- appears as if shifted to the right by 2
duplicate $ ix 2 (duplicate s)
  == (2 :> 3 :> 4 :> …) -- s₀
  :> (3 :> 4 :> 5 :> …) -- s₁
  :> (4 :> 5 :> 6 :> …) -- s₂
  :> …
```

>`duplicate` lifts a query into a mutation.

We can implement `drop` in terms of `duplicate` and `ix`:

```hs
ix :: Int -> Stream a -> a
ix 0 (x :> _)  = x
ix n (_ :> xs) = ix (n - 1) xs

drop :: Int -> Stream a -> Stream a
drop n s = ix n (duplicate s)
```

But can we go the other way? If we have some way to edit the entire structure, but we want to turn it into a query that compresses it down into a single element. For example, can we define `ix` in terms of `drop`?

```hs
drop :: Int -> Stream a -> Stream a

ix   :: Int -> Stream a ->        a
ix n s = extract (drop n s)

-- this would be an infinite loop
-- ix 0 (drop n s)
-- so we make `ix 0` special and call it extract
extract = ix 0

-- extract :: Comonad => w a -> a
extract :: Stream a -> a
extract (x :> _) = x
```

The index fucntion, `ix`, is special - it is the root of the structure (stream), it extracts the focused slot. And that's exactly the name it gets, `extract`. It is a no-op query.
