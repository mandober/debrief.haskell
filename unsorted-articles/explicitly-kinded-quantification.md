# 6.11.3. Explicitly-kinded quantification — Glasgow Haskell Compiler 9.3.20210805 User's Guide

> Allow explicit kind signatures on type variables.

`KindSignatures`[¶](#extension-KindSignatures "Permalink to this definition")

<table><colgroup><col> <col></colgroup><tbody><tr><th>Since:</th><td>6.8.1</td></tr></tbody></table>

Allow explicit kind signatures on type variables.

Haskell infers the kind of each type variable. Sometimes it is nice to be able to give the kind explicitly as (machine-checked) documentation, just as it is nice to give a type signature for a function. On some occasions, it is essential to do so. For example, in his paper “Restricted Data Types in Haskell” (Haskell Workshop 1999) John Hughes had to define the data type:

data Set cxt a \= Set \[a\]
               | Unused (cxt a \-> ())

The only use for the `Unused` constructor was to force the correct kind for the type variable `cxt`.

GHC now instead allows you to specify the kind of a type variable directly, wherever a type variable is explicitly bound, with the extension [`KindSignatures`](#extension-KindSignatures).

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