# Containers

Informally, a container is a functor with a "shapes and positions (indices)" presentation: the contained values are given as the image of a function from indices, but the type of indices depends on the choice of the shape.

For example, finite lists can be seen as mappings from an `n`-element set to a list's elements, once you've chosen the shape `n`, i.e. list's length.

```js
N = {0,1,2,3}
L = {a,b,c,d}

f :: N -> L

l₁ = [d,b,c,a]

0 -----> d
1 -----> b
2 -----> c
3 -----> a
```

If functor `f` is a container, then its shape set will be isomorphic to `f ()`, or what you get when you choose some boring elements that just mark their position. It's the dependency of the position set on the shape that makes the concept a little tricky to express in Haskell.

https://pigworker.wordpress.com/2015/06/06/hasochistic-containers-a-first-attempt/
