# Immutability and mutability

https://en.wikibooks.org/wiki/Haskell/Mutable_objects




Haskell is a pure functional programming language, with immutable values - any defined variable (strictly speaking, unlike *variable declaration*, *variable definition* implies binding a declared variable to a value) is immutable. Variable declaration behaves more like a constant declaration in other languages.

Languages have different levels of mutability: scalar values like numbers are almost always immutable. Some compound values may be mutable and some immutable (e.g. in Python, tuples are immutable, while lists are mutable). Variables, on the other hand, bind a value, and also have different aspects of mutability. Some PL insists that you declare a variable as im/mutable. Languages also have keywords that set the mutability of a variable's contents. Some PL allow variables to change the binding value (e.g. var x = 5; x = 6), and some even allow changing the type of variable (e.g. let x = 4; x = "abc"). There are many aspects of mutability when it comes to a variable that binds a compound value: changing the entire contents, changing only a part of it.

In Haskell, everything is immutable, so we needn't worry about this. And since everything is immutable, we can speak of *mutatating a value* when we actually mean *updating a value* - so we get a completely new value with some of its parts updated.

For example, creating a new list of 3 elements, `xs = [1, 2, 3]`, then changing the first into 9, leaves us with a new list `xs' = [9, 2, 3]`. However, copying a list every time a part of it changes would be incredibly wasteful and inefficient. Thus internally, after the first list `xs` is created (lists are Lisp-inspired sequences of cons-cells), the changed lists `xs'` is realized by creating only one new cons-cell that holds the value 9 and points at the second node of the original list `xs`.


A *cons-cell* is a two-field data structure with each field holding a pointer. The first field, commonly called `car`, is more complicated because it may hold a value directly or indirectly (via a pointer), or it may hold a pointer to another cons-node.
- immediate value (up to word-size) in case of a plain, flat list
- indirect value (if larger than word), i.e. pointer to the heap allocated value
- pointer to a node (in case of nested lists)


may in fact hold a word-sized value directly, or, for larger values, a pointer to the value on the heap. Also, in case of list of lists, the `car` may hold a pointer to a cons-node. The second field, commonly called `cdr`, holds a pointer to a cons-node. There is also a special value that terminates a list, usualy called `nil`.



: the first field, commonly called `car` hold a value (payload) or a point

```
    cons-cell
╭───────┬───────╮
│ car   │   cdr │
│  ●    │    ○  │
╰───────┴───────╯
```

Sequence of cons-cell

```
    cons-cell
╭───────┬───────╮
│ car   │   cdr │
│  ●    │    ○  │
╰───────┴───────╯
```



*Functional purity* is a defining characteristic of Haskell, one which leads to many of its strengths. As such, language and ecosystem encourage eschewing *mutable state* altogether. Thanks to tools such as the State monad, which allows us to keep track of state in a convenient and functionally pure way, and efficient *immutable data structures* like the ones provided by the containers and unordered-containers packages, Haskell programmers can get by perfectly fine with complete immutability in the vast majority of situations.

However, under select circumstances using *mutable state* is the most sensible option. One might, for instance, be interested in:
- From Haskell code, using a library written in another language which assumes mutable state everywhere. This situation often arises with event-callback GUI toolkits.
- Using Haskell to implement a language that provides imperative-style mutable variables.
- Implementing algorithms that inherently require destructive updates to variables.
- Dealing with volumes of bulk data massive enough to justify squeezing every drop of computational power available to make the problem at hand feasible.

Any general-purpose programming language should be able to deal with such tasks. Haskell is no different: there are not only ways to create mutable objects, but it is possible to do it in such way to keep mutability under control in a setting where immutability is the default.
