# Replacing functions with data

Bartosz Milewski - Replacing functions with data @Haskell Love conference
https://youtu.be/wppzFzzD4b8?list=PLBqWQH1MiwBSK9wuaATNS701c43VYVTuc
by Bartosz Milewski on 2020-08-13

Haskell Love conference: the entire playlist
https://www.youtube.com/watch?v=wppzFzzD4b8&list=PLBqWQH1MiwBSK9wuaATNS701c43VYVTuc&index=3



*Replacing functions with data*: this talk is about functional programming, where functions are first-class citizens. This is due to the *adjunction* between the product and the exponential. It's little known that, when you relax this adjunction, you can replace some functions with data in the process called **defunctionalization**. I'll show you how to defunctionalize tree traversal in Haskell.

**adjunction**
1. (math) The joining of two sets which without overlapping jointly constitute a larger set, or the relation between two such sets. a ∪ {b}
2. (logic) The asserting in a single formula of two previously asserted formulae.

## Adjunction (adjoint functors)

https://en.wikipedia.org/wiki/Adjoint_functors
https://bartoszmilewski.com/2016/04/18/adjunctions/
https://ncatlab.org/nlab/show/adjunction
https://www.math3ma.com/blog/what-is-an-adjunction-part-1
https://www.math3ma.com/blog/what-is-an-adjunction-part-2
https://www.youtube.com/watch?v=loOJxIOmShE
https://www.youtube.com/watch?v=TnV9SQGPcLY
https://www.youtube.com/watch?v=TNtntlVo4LY
https://www.youtube.com/watch?v=AppzvbDLxBw


# Replacing functions with data: code example

In this quick impl of tree, we're concerned with the show1 function - it is not tail recursive, so we wear our wedding gown with a frown.

```hs
data Tree = Leaf String | Node Tree String Tree deriving (Eq, Ord)

t1 :: Tree
t1 = Node (Node (Leaf "1") "2" (Leaf "3")) "4" (Leaf "5")

show1 :: Tree -> String
show1 (Leaf s) = s
show1 (Node lft s rgt) =
    show1 lft ++ s ++ show1 rgt

test = show1 t1 -- "12345"
```

But not to worry, we can make it tail recursive by passing it a continuation; it's a function that will accept the value intended to be returned, which, originally was a String, and process it into some value `a` (we can't tell in advance what it will be, so the return type is parameterized). That same value `a` will also be the return value of the `show` function.

```hs
show2 :: Tree -> (String -> a) -> a
show2 (Leaf s)         k = k s
show2 (Node lft s rgt) k =
    show2 lft (\ls ->
        show2 rgt (\rs ->
            k (ls ++ s ++ rs)))


show t = show2 t id
```

We pass in a continuation, `k`, and, in the body of show, we call it with the leaf value `s` in the base case. In the rec case, we place the continuation (let it be known as the A continuation). The A continuation actually has another continuation (let's call it B) nested inside it (another lambda; continuations are usually lambdas).

```hs
-- continuation A:
\ls ->
    show2 rgt (
        \rs ->
            k (ls ++ s ++ rs)
    )

-- continuation B:
\rs ->
    k (ls ++ s ++ rs)
```

So, we start the inductive case as before, with the rec call `show lft ...`, but while originally we had `show lft ++ s ++ show rgt`, the function now calls itself, passing thyself the left subtree, as `lft`, **AND** another arg, i.e. the cont A, as `(\ls -> show2 rgt (\rs -> k (ls ++ s ++ rs)))`. Since that recursive call is the only thing happening, the show fn officially earns the tail recursive stripes thereafter.

That aside, wtf is happening thereafter indeed? When the show fn finishes traversing the left subtree, it will have the string formed from the left subtree's values. The show fn then calls and passes this string to the A cont, which binds the string to the param `ls`; from within the A cont the show function is called again, this time with the `rgt` arg (the right subtree) **AND** with the continuation B, `\rs -> k (ls ++ s ++ rs)`.

Finally, when the traversal of the right subtree is done and the string of right subtree's values assembled, the **continuation `k`** is then called on the concat'ed string values `ls` ++ `s` ++ `rs` (`s` is the value of/at the current root node that is directly available). This continuation is the one that the caller of the `show` function must provide; by passing in the `id` function (the so-called *trivial continuation*), the `show` fn behaves as before; however, besides obtaining tail-recursion, other options also open up when a more interesting continuation is passed in.

There were 3 continuations total, but sometimes only the one supplied by the caller (`k`) qualifies as a continuation, while the other two (A and B conts.), despite being true continuations, are considered to be more of an impl detail (since they're in the implementor's domain, i.e. not explicitly seen, or maybe even entirely unbeknownst to the caller).

## Defunctionalisation

Instead of the three lambdas, we could have also worked with three named functions, but that brings up the point about lambdas: they are something more then simple functions, they are -- closures! And closures have the ability to capture variables from the env. That is a feature we need to take into account if we want to rid of the lambdas (but why would we ever want to rid ourselves of lambdas, forgod's sake?! Lambdas were introduced into PLs precisely because they offer a more concise and elegant way of expression). It seems we now found ourselves in the proof-of-concept waters, leaving us no choice but to get on with the program. The program is: how to rid oneselves of lambdas by learning a pattern we're never gonna use - *defunctionalisation*. Hell, since we know everything about Haskell, all the milions of things of varying levels of importance, whydontwe endulge ourselves and learn this fucking unusable thing first! Who knows what crucial insight about Haskell, PLs, environments and life in general, will that wondeful experince bring, hooray no nay.

The caller's cont was just the id func, so we need not worry about that one. The other two conts can now officially get their names, A and B. Of course, we can't just stick a name in front of a lambda expression, first we need to make sure all free vars they have are parameterized (and see that the `k` param comes last; it's the most convenint that way). While we're at introducing new params to our de-lambded functions, we might as well convert lambda params into proper function params (althout it makes no difference from the functional point of view, it may matter if a particular order of formal params is desired).

```hs
-- continuation A as raw lambda, first we bind it:
a = \ls -> show2 rgt (\rs -> k (ls ++ s ++ rs))
-- then we parameterize the free vars:
contA rgt s k ls = show2 rgt (\rs -> k (ls ++ s ++ rs))



-- continuation B as lambda:
\rs -> k (ls ++ s ++ rs)
-- bind it:
contB k ls s rs = k (ls ++ s ++ rs)
-- parameterize free vars:
contB k ls s rs = k (ls ++ s ++ rs)
```
