# Recursion schemes

https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

In 1991, Erik Meijer, Maarten Fokkinga, and Ross Paterson published the paper `Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire`. The authors used category theory to express a set of simple, composable combinators, called *recursion schemes*, that automate the process of traversing and recursing through nested data structures.

Traversing a recursive structure is a very common task, so it proves beneficial to abstract it. By generalizing traversals we can then replace numerous type-specific operations with a single one. By decoupling recursion from traversal, we can focus entirely on the core behavior of operations over data structures. Recursion schemes provide an orderly and predictable way of traversing them.
