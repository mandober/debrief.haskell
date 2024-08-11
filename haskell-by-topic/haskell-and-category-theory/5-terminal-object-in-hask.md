# Haskell :: Haskell and CT :: Terminal object

Terminal object in a category is one which has a unique incoming arrow from any other object in the category, including from itself via identity arrow.

Terminal objects
- in Set: `{⋆}`
- in Hask: `Unit` or `()` (canonical)
- in logic: `⊤`
- denoted: `1`, `𝟙`

In Haskell, the `()` type (called unit type) is the *canonical terminal object*. It could have been defined like `Unit` below:

```hs
data () = ()
data Unit = Unit
```

In `Set`, the terminal object is any singleton set. Since all such sets are isomorphic, we usually talk about "the" terminal object. In fact, any two sets with the same cardinality are isomorphic (at least if they are finite). A singleton set is usually denoted by `{⋆}`, and its sole inhabitant (if anything) is called "star".

The big role that the terminal object has in a category is its capability to select subobjects, i.e. elements of objects. It is a *subobject selector*. Noramlly, objects in a ctegory are opaque, featureless blobs. Yet, when we know what they really are (like in `Set`, where we know objects are sets with elements inside), we can rely on the subobject selector to derive information about sets (objects). This is because there will be exactly 2 arrows from `{⋆}` to an object that has two elements: an arrow to select the first element (e.g. 'true') and another to select the second element (e.g. "false"). Since all two element sets are isomorphic, we might as well call any 2-element set *the Boolean set*, `𝔹`.

In Haskell, the subobject selector role of the terminal object, i.e. unit type, is mostly seen as a way to reconsile categorical selection and nullary functions. Namely, `() -> a` ≅ `a`, i.e. should be treated as equivalent.
