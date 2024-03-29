# Explicitly-kinded quantification

Allow explicit kind signatures on type variables.

Haskell infers the kind of each type variable. Sometimes it is nice to be able to give the kind explicitly as (machine-checked) documentation, just as it is nice to give a type signature for a function. On some occasions, it is essential to do so.

For example, in his paper "Restricted Data Types in Haskell" (Haskell Workshop 1999) John Hughes had to define the following data type that had a data ctor named `Unused` because that was the only way to force the correct kind for the type variable `cxt`.

```hs
data Set cxt a = Set [a] | Unused (cxt a -> ())
```

With `KindSignatures`, GHC allows specifying the kind of a type variable directly, wherever a type variable is explicitly bound.

This extension enables kind signatures in the following places:

*   `data` declarations:
    
    data Set (cxt :: Type \-> Type) a \= Set \[a\]
    
*   `type` declarations:
    
    type T (f :: Type \-> Type) \= f Int
    
*   `class` declarations:
    
    class (Eq a) \=> C (f :: Type \-> Type) a where ...
    
*   `forall`‘s in type signatures:
    
    f :: forall (cxt :: Type \-> Type). Set cxt Int
    

The parentheses are required.

As part of the same extension, you can put kind annotations in types as well. Thus:

f :: (Int :: Type) \-> Int
g :: forall a. a \-> (a :: Type)

The syntax is

atype ::= '(' ctype '::' kind ')

The parentheses are required.


[Source](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/kind_signatures.html)
