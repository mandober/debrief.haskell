# Newtype coercions

A *newtype* is a user-defined algebraic datatype with exactly one constructor that takes exactly one argument. For example:

```hs
newtype HTML = MkHTML String
```

This type declaration creates a **generative abstraction**; the `HTML` type is new in the sense that it is not equal to any existing type. We call the argument type, `String`, the *representation type*.

*A newtype is isomorphic to its representation type*, so the compiler uses the same in-memory format for values of both types. Thus, creating a value of a newtype (by applying `MkHTML` data ctor to a string) is free at runtime, as is unpacking it (i.e. pattern-matching on it). `HTML` and `String` are represented identically at runtime and the `MkHTML` constructor compiles into the identity function. The primary benefit of a newtype is that the type checker treats the newtype and its representation type as completely distinct types, so you can't accidentally confuse `HTML` values with `String` values. Therefore, newtypes are a *zero-cost abstraction*: they provide a convenient compile-time distinction at no cost to runtime efficiency.

A newtype exported from a module, without also exporting its data ctor, is an *abstract datatype* since clients don't have access to its representation. By exporting the type HTML without the `MkHTML` data constructor, the module ensures that the type is abstract, and the clients cannot make arbitrary strings into `HTML` (thereby preventing e.g. cross-site scripting attacks). Outside the module, the only way to construct a value of the `HTML` type would be to use a smart ctor provided by the author which usually enforces some type invariants (e.g. perhaps it runs the arg string through a text sanitizer).

However, within the defining module, the newtype is a *translucent abstraction*: you can see through it with some effort. The *safe coercions* (Breitner et al. 2016) GHC extension reduces this effort by exposing the `coerce` primitive. For example, since `HTML` and `String` have the same memory representation, so do the lists `[HTML]` and `[String]`. Thus, we can define a zero-cost operation that converts the former to the latter by **coercing between representationally equal types**.

```hs
unpackList :: [HTML] -> [String]
unpackList = coerce
```

However, `coerce` must be used with care because not every structure is appropriate for such conversion. For example, coercing a `Map HTML Int` to a `Map String Int` would be disastrous if the ordering relation used for keys differs between `HTML` and `String`. Even worse, allowing coercions on types that use the *type family* feature (Chakravarty et al. 2005) leads to unsoundness.

Haskell resolvs this problem by introducing the *role system*. The *role annotations* are associated with type ctors to indicate whether it is appropriate to lift newtype-coercions through abstract structures (such as `Map`).

The key idea of *safe coercions* is recognizing two notions of type equality:
- **nominal equality**
- **representational equality**

Nominal type equality is the usual definition of type equality that can distinguish between the types `HTML` and `String`. On the other hand, representational equality identifies types that are suitable for coercion.

When a type variable, that is a type argument to a type ctor, is not congruent with respect to representational equivalence, placing a role annotation can prohibit the derivation of these undesired equalities.

## Newtypes provide zero-cost abstractions

We're considering this `HTML` example module:

```hs
module Html (HTML, text, unHTML) where

newtype HTML = MkHTML String

unHTML :: HTML -> String
unHTML (MkHTML s) = s

text :: String -> HTML
text s = MkHTML (escapeSpecialCharacters s)

instance IsString HTML where
  fromString = text
```

`HTML` is a newtype and its representation type is `String`. This means that both `HTML` and `String` are represented identically at runtime and that the `MkHTML` constructor compiles into the identity function. However, the type system keeps `HTML` and `String` separate: a function that expects an `HTML` will not accept something of type `String`.

The module exports the type `HTML` but not its data constructor `MkHTML`. Accordingly, outside the module, the only way to construct a value of this type is to use the `text` function, which enforces the invariant of the data structure. By exporting the type `HTML` without its data constructor, the module ensures that the type is abstract, clients cannot make arbitrary strings into `HTML`.

However, within the module, the author would surely want to reuse functions that work with Strings for his `HTML` type, even if those functions actually work with, e.g. `[String]`, `Maybe String`, etc. To support this reuse, certain types, including functions (->) and lists [], allow us to lift coercions between `String` and `HTML`. For example, suppose we wish to break up chunks of HTML into their constituent lines. We define

```hs
linesH :: HTML -> [HTML]
linesH = coerce lines
```

Using Haskell's standard `lines :: String -> [String]` function, we have now, with minimal effort, lifted its action to work over `HTML`. Critically, the use of coerce above is allowed only when the `MkHTML` constructor is in scope; in other words, `linesH` can be written in the `Html` module but not outside it. In this way, the author of `HTML` has a chance to ensure that any invariants of the `HTML` type are maintained if an `HTML` chunk is split into lines.
