# Newtype

- newtype
- type
- newtype and type couples
- newtype, type and "bare" type triples


## Newtype wrapper

Consider a type defined using the `newtype` keyword, with an accessor function also defined, e.g.

```hs
               ┌ declaration   application ┐
type-name ┐    │       occurrence of       │
          │    │        type-params        │
type-ctor ┤    │                           │
          │    │ data-ctor   accessor      ├─ wrapped-type
keyword   │    │      │        ├─ label    ├─ function-type
┌──┴──┐ ┌─┴─┐ ┌┴┐   ┌─┴─┐   ┌──┴───┐    ┌──┴──────┐
newtype State s a = State { runState :: s -> (a, s) }
```

An accessor, especially with newtypes that can only ever have a single field, is a function that given a value of the type being defined (`State s a`), returns the type that is inside the field, i.e. the type being wrapped (`s -> (a, s)`). In other words, it peels off the data ctor (`State`), producing the unwrapped type.

But where are the implementations od these functions? Both, the data ctor and the field accessor (`State` and `runState`) are indeed functions, but their definitions are nowhere to be found. Despite the slight obscurity, the type signatures can be inferred (by people, that is), but even though the implementations are completely lacking, the compiler is not pulling the usual: "The type signature for <name> lacks an accompanying binding".

Consisting just of function applications, the definitions of these functions is automatically constructed. However, there are plenty other polymorphic functions whose type signatures similarly force the definition, but nevertheless it is required to manually write one out.

Moreover, as with all automatic code derivations, there is the question what exactly does the derived implementation looks like regarding some more complicated data types (e.g. that have complex pattern mathching).

Smart ctors are a Haskell's idiom that addresses this situation in face of demand for more feature-full type constructions. When a data ctor is thus redefined as a separate function, it is the one that's exported, while the original data ctor is left private.
