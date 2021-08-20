# Memory representation

Consider a type like `Maybe`:

```hs
data Maybe a = Nothing | Just a
```

At RT, the type `Maybe` is represented by a pointer. Its two values, tagged by the two data ctors, `Nothing` and `Just`, are further represented by pointers. Actually, if the value of `Maybe a` is `Nothing`, the `Nothing` is repr by a particular special-purpose memory address (perhaps, the same one used to repr NULL), so the entire `Maybe :: Nothing` value is the pointer to that special address. All the nullary data ctors are repr by that same special memory address by GHC; all nullary data ctors point to that same address. This means the value of `Maybe a` is a pointer, and since it holds a `Nothing`, it points to the special memory address by which GHC can easily identify that this Maybe indeed holds a `Nothing`. So, to test whether `Maybe a` is `Nothing`, GHC can just test if that `Maybe a` points to that special address.

[since it is immutable, like all other values, Maybe probably repr `Nothing` by collapsing into a pointer to NULL. And it repr `Just a` by a pointer to some heap allocated value; it cannot collapse in this case coz the size of the payload is unknown. If Maybe were mutable, then it would make sense not to collapse it in the case of Nothing coz it could be changed into a Just, requiring it to always have a pointer. What are you on about? A Maybe value has, or rather *is*, a pointer in both cases! If Nothing, that pointer points to NULL; if Just, then it points to some heap memory address].

If it does not, it means we have the `Just a` value. This data ctor is different because it actually carries some payload of type `a`. There has to be some heap-allocated memory to hold the value of that type `a`. So, in this case, the value of `Maybe a` is a pointer to that allocated address. Maybe cannot hold the payload directly (as in, Maybe collapses into the value it holds), rather it holds a pointer because the type `a` doesn't have a fixed size, it can be anything at all.
