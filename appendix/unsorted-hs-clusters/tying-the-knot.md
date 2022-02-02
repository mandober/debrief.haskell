# Tying the Knot

An overview of essential Haskell technique that let's us avoid mutation (`IORef` and such) and code the DLL only using pure code.


This post has nothing to do with marriage. Tying the knot is, in my opinion at least, a relatively obscure technique you can use in Haskell to address certain corner cases. I've used it myself only a handful of times, one of which I'll reference below. I preface it like this to hopefully make clear: tying the knot is a fine technique to use in certain cases, but don't consider it a general technique that you should need regularly. It's not nearly as generally useful as something like [Software Transactional Memory](https://www.fpcomplete.com/haskell/library/stm/).

That said, you're still interested in this technique, and are still reading this post. Great! Let's get started where all bad Haskell code starts: C++.


## Doubly linked lists

Typically I'd demonstrate imperative code in Rust, but [it's not a good idea for this case](https://rust-unofficial.github.io/too-many-lists/). So we'll start off with a very simple doubly linked list implementation in C++. And by "very simple" I should probably say "very poorly written," since I'm out of practice.

![Rusty C++](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/images/haskell/cpp-is-rusty.png)

Anyway, reading the entire code isn't necessary to get the point across. Let's look at some relevant bits. We define a node of the list like this, including a nullable pointer to the previous and next node in the list:

    template <typename T> class Node {
    public:
      Node(T value) : value(value), prev(NULL), next(NULL) {}
      Node *prev;
      T value;
      Node *next;
    };
    

When you add the first node to the list, you set the new node's previous and next values to `NULL`, and the list's first and last values to the new node. The more interesting case is when you already have something in the list. To add a new node to the back of the list, you need some code that looks like the following:

    node->prev = this->last;
    this->last->next = node;
    this->last = node;
    

For those (like me) not fluent in C++, I'm making three mutations:

1.  Mutating the new node's `prev` member to point to the currently last node of the list.
2.  Mutating the currently last node's `next` member to point at the new node.
3.  Mutating the list itself so that its `last` member points to the new node.

Point being in all of this: there's a lot of mutation going on in order to create a double linked list. Contrast that with singly linked lists in Haskell, which are immutable data structures and require no mutation at all.

Anyway, I've written my annual quota of C++ at this point, it's time to go back to Haskell.

RIIH (Rewrite it in Haskell)
----------------------------

Using `IORef`s and lots of `IO` calls everywhere, it's possible to reproduce the C++ concept of a mutable doubly linked list in Haskell. Full code is [available in a Gist](https://gist.github.com/snoyberg/5de410aba87a4208b7c701e954c61d9d), but let's step through the important bits. Our core data types look quite like the C++ version, but with `IORef` and `Maybe` sprinkled in for good measure:

    data Node a = Node
        { prev  :: IORef (Maybe (Node a))
        , value :: a
        , next  :: IORef (Maybe (Node a))
        }
    
    data List a = List
        { first :: IORef (Maybe (Node a))
        , last :: IORef (Maybe (Node a))
        }


And adding a new value to a non-empty list looks like this:

    node <- Node <$> newIORef (Just last') <*> pure value <*> newIORef Nothing
    writeIORef (next last') (Just node)
    writeIORef (last list) (Just node)



Notice that, like in the C++ code, we need to perform mutations on the existing node and the `last` member of the list.

This certainly works, but it probably feels less than satisfying to a Haskeller:

*   I don't love the idea of mutations all over the place.
*   The code looks and feels ugly.
*   I can't access the values of the list from pure code.

So the challenge is: can we write a doubly linked list in Haskell in pure code?


Defining our data
-----------------

I'll warn you in advance. Every single time I've written code that "ties the knot" in Haskell, I've gone through at least two stages:

1.  This doesn't make any sense, there's no way this is going to work, what exactly am I doing?
2.  Oh, it's done, how exactly did that work?

It happened while writing the code below. You're likely to have the same feeling while reading this of "wait, what? I don't get it, huh?"

Anyway, let's start off by defining our data types. We didn't like the fact that we had `IORef` all over the place. So let's just get rid of it!

    data Node a = Node
        { prev  :: Maybe (Node a)
        , value :: a
        , next  :: Maybe (Node a)
        }
    
    data List a = List
        { first :: Maybe (Node a)
        , last :: Maybe (Node a)
        }
    

We still have `Maybe` to indicate the presence or absence of nodes before or after our own. That translation is pretty easy. The problem is going to arise when we try to build such a structure, since we've seen that we need mutation to make it happen. We'll need to rethink our API to get going.

Non-mutable API
---------------

The first change we need to consider is getting rid of the _concept_ of mutation in the API. Previously, we had functions like `pushBack` and `popBack`, which were inherently mutating. Instead, we should be thinking in terms of immutable data structures and APIs.

We already know all about singly linked lists, the venerable `[]` data type. Let's see if we can build a function that will let us construct a doubly linked list from a singly linked list. In other words:

    buildList :: [a] -> List a
    

Let's knock out two easy cases first. An empty list should end up with no nodes at all. That clause would be:

    buildList [] = List Nothing Nothing
    

The next easy case is a single value in the list. This ends up with a single node with no pointers to other nodes, and a `first` and `last` field that both point to that one node. Again, fairly easy, no knot tying required:

    buildList [x] =
        let node = Node Nothing x Nothing
         in List (Just node) (Just node)
    

OK, that's too easy. Let's kick it up a notch.

Two-element list
----------------

To get into things a bit more gradually, let's handle the two element case next, instead of the general case of "2 or more", which is a bit more complicated. We need to:

1.  Construct a first node that points at the last node
2.  Construct a last node that points at the first node
3.  Construct a list that points at both the first and last nodes

Step (3) isn't too hard. Step (2) doesn't sound too bad either, since presumably the first node already exists at that point. The problem appears to be step (1). How can we construct a first node that points at the second node, when we haven't constructed the second node yet? Let me show you how:

    buildList [x, y] =
        let firstNode = Node Nothing x (Just lastNode)
            lastNode = Node (Just firstNode) y Nothing
         in List (Just firstNode) (Just lastNode)
    

If that code doesn't confuse or bother you you've probably already learned about tying the knot. This seems to make no sense. I'm referring to `lastNode` while constructing `firstNode`, and referring to `firstNode` while constructing `lastNode`. This kind of makes me think of an [Ouroboros](https://en.wikipedia.org/wiki/Ouroboros), or a snake eating its own tail:

In a normal programming language, this concept wouldn't make sense. We'd need to define `firstNode` first with a null pointer for `next`. Then we could define `lastNode`. And then we could mutate `firstNode`'s `next` to point to the last node. But not in Haskell! Why? Because of _laziness_. Thanks to laziness, both `firstNode` and `lastNode` are initially created as thunks. Their contents need not exist yet. But thankfully, we can still create pointers to these not-fully-evaluated values.

With those pointers available, we can then define an expression for each of these that leverages the pointer of the other. And we have now, successfully, tied the knot.

Expanding beyond two
--------------------

Expanding beyond two elements follows the exact same pattern, but (at least in my opinion) is significantly more complicated. I implemented it by writing a helper function, `buildNodes`, which (somewhat spookily) takes the previous node in the list as a parameter, and returns back the next node and the final node in the list. Let's see all of this in action:

```hs

    buildList (x:y:ys) =
        let firstNode = Node Nothing x (Just secondNode)
            (secondNode, lastNode) = buildNodes firstNode y ys
         in List (Just firstNode) (Just lastNode)
    
    -- | Takes the previous node in the list, the current value, and all following
    -- values. Returns the current node as well as the final node constructed in
    -- this list.
    buildNodes :: Node a -> a -> [a] -> (Node a, Node a)
    buildNodes prevNode value [] =
        let node = Node (Just prevNode) value Nothing
         in (node, node)
    buildNodes prevNode value (x:xs) =
        let node = Node (Just prevNode) value (Just nextNode)
            (nextNode, lastNode) = buildNodes node x xs
         in (node, lastNode)
```


Notice that in `buildList`, we're using the same kind of trick to use `secondNode` to construct `firstNode`, and `firstNode` is a parameter passed to `buildNodes` that is used to construct `secondNode`.

Within `buildNodes`, we have two clauses. The first clause is one of those simpler cases: we've only got one value left, so we create a terminal node that points back at previous. No knot tying required. The second clause, however, once again uses the knot tying technique, together with a recursive call to `buildNodes` to build up the rest of the nodes in the list.

The full code is [available as a Gist](https://gist.github.com/snoyberg/876ad1ad0f106c80239bf098a6965a53). I recommend reading through the code a few times until you feel comfortable with it. When you have a good grasp on what's going on, try implementing it from scratch yourself.

Limitation
----------

It's important to understand a limitation of this approach versus both mutable doubly linked lists and singly linked lists. With singly linked lists, I can easily construct a new singly linked list by `cons`ing a new value to the front. Or I can drop a few values from the front and cons some new values in front of that new tail. In other words, I can construct new values based on old values as much as I want.

Similarly, with mutable doubly linked lists, I'm free to mutate at will, changing my existing data structure. This behaves slightly different from constructing new singly linked lists, and falls into the same category of mutable-vs-immutable data structures that Haskellers know and love so well. If you want a refresher, check out:

*   [Data structures](https://www.fpcomplete.com/haskell/tutorial/data-structures/)
*   [vector](https://www.fpcomplete.com/haskell/library/vector/)
*   [Mutable variables](https://www.fpcomplete.com/haskell/tutorial/mutable-variables/)

None of these apply with a tie-the-knot approach to data structures. Once you construct this doubly linked list, it is locked in place. If you try to prepend a new node to the front of this list, you'll find that you cannot update the `prev` pointer in the old first node.

There is a workaround. You can construct a brand new doubly linked list using the values in the original. A common way to do this would be to provide a conversion function back from your `List a` to a `[a]`. Then you could append a value to a doubly linked list with some code like:

    let oldList = buildList [2..10]
        newList = buildList $ 1 : toSinglyLinkedList oldList
    

However, unlike singly linked lists, we lose all possibilities of data sharing, at least at the structure level (the values themselves can still be shared).

Why tie the knot?
-----------------

That's a cool trick, but is it actually useful? In some situations, absolutely! One example I've worked on is in the [xml-conduit](https://www.stackage.org/package/xml-conduit) package. Some people may be familiar with XPath, a pretty nice standard for XML traversals. It allows you to say things like "find the first `ul` tag in document, then find the `p` tag before that, and tell me its `id` attribute."

A simple implementation of an XML data type in Haskell may look like this:

    data Element = Element Name (Map Name AttributeValue) [Node]
    data Node
        = NodeElement Element
        | NodeContent Text
    

Using this kind of data structure, it would be pretty difficult to implement the traversal that I just described. You would need to write logic to keep track of where you are in the document, and then implement logic to say "OK, given that I was in the third child of the second child of the sixth child, what are all of the nodes that came before me?"

Instead, in `xml-conduit`, we use knot tying to create a data structure called a [`Cursor`](https://www.stackage.org/haddock/nightly-2021-05-23/xml-conduit-1.9.1.1/Text-XML-Cursor.html#t:Cursor). A `Cursor` not only keeps track of its own contents, but also contains a pointer to its parent cursor, its predecessor cursors, its following cursors, and its child cursors. You can then traverse the tree with ease. The traversal above would be implemented as:

    #!/usr/bin/env stack
    -- stack --resolver lts-17.12 script
    {-# LANGUAGE OverloadedStrings #-}
    import qualified Text.XML as X
    import Text.XML.Cursor
    
    main :: IO ()
    main = do
        doc <- X.readFile X.def "input.xml"
        let cursor = fromDocument doc
        print $ cursor $// element "ul" >=> precedingSibling >=> element "p" >=> attribute "id"
    

You can test this out yourself with this sample input document:

    <foo>
        <bar>
            <baz>
                <p id="hello">Something</p>
                <ul>
                    <li>Bye!</li>
                </ul>
            </baz>
        </bar>
    </foo>
    

Should I tie the knot?
----------------------

_Insert bad marriage joke here_

Like most techniques in programming in general, and Haskell in particular, it can be tempting to go off and look for a use case to throw this technique at. The use cases definitely exist. I think `xml-conduit` is one of them. But let me point out that it's the _only_ example I can think of in my career as a Haskeller where tying the knot was a great solution to the problem. There are similar cases out there that I'd include too (such as JSON document traversal).

Is it worth learning the technique? Yeah, definitely. It's a mind-expanding move. It helps you internalize concepts of laziness just a bit better. It's really fun and mind-bending. But don't rush off to rewrite your code to use a relatively niche technique.

If anyone's wondering, this blog post came out of a question that popped up during a Haskell training course. If you'd like to come learn some Haskell and dive into weird topics like this, come find out more about [FP Complete's training programs](https://www.fpcomplete.com/training/). We're gearing up for some intermediate Haskell and Rust courses soon, so add your name to the list if you want to get more information.

Do you like this blog post and need help with DevOps, Rust or functional programming? [Contact us](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/contact-us/).


[Source](https://www.fpcomplete.com/blog/tying-the-knot-haskell/)
