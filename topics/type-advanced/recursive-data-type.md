# Recursive data types












## Recursive structures

In imperative languages, like C, we'd have to explicitly use pointers to design a recursive data structure such as a linked list.

```cpp
struct List {
  void *head;
  List *tail;
}

List* repeat(void *x) {
  List *xs = new List;
  xs->head = x;
  xs->tail = xs; /* close the loop */
  return xs;
}
```

There are mutable references in Haskell, but there is lazy evaluation, recursive let and "tying the knot" technic, and we can even represent infinite data structures.

```hs
repeat :: a -> [a]
-- 1
repeat x = x : repeat x
-- 2: tying the knot
repeat x = let xs = x:xs in xs
```

### Doubly-linked lists

We can't just extend the definition of a singly-linked list.

```hs
data List a = Nil | Cons a (List a)
data DLL  a = Nil | Node a (DLL  a) (DLL a)

data DLL a
  = Nil
  | Node
  { item :: a
  , prev :: DLL a
  , next :: DLL a
  }
```

Values of this type will not persist well, we won't be able to build them
incrementally. We'll have to build them all in one go with "tying the knot" technic.

```hs
mkList :: [a] -> DLL a
mkList [] = Nil
mkList (x:xs) = ???
```
