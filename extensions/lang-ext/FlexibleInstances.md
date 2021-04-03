# FlexibleInstances

https://jsdw.me/posts/haskell-language-extensions/#flexibleinstances

* instance declarations need to take the form `T a1 a2 ... an` where `a`s are type variables, but `String` = `[Char]` = `[] Char`, and `Char` is not a type variable but a fixed type. So basically, `[] a` is fine, but `[] Char` is not. FlexibleInstances removes this restriction, and more generally adds flexibility to the format that your instances can take.

* Alternatively, you can avoid this pargma by enforcing that `a` is `Char` some other way, via some constraint, for example:

```hs
class CharType a
instance CharType Char

instance CharType a => Truthy [a] where
    truthy s = length s /= 0
```


* If you want to do anything other than `T a1 a2 ... an` (`a`s have to be type variables) than you need flexible instances.

* All instance types must be of the form `(T a1 ... an)` where `a1 ... an` are *distinct type variables*, and each type variable appears at most once in the instance head. Use `-XFlexibleInstances` if you want to disable this.

https://stackoverflow.com/questions/20145943/flexible-instances-needed

* (or) Turn on GADTs or TypeFamilies and write:   
`instance a ~ Ptr () => Convertible Variable (IO a) where ...`


# FlexibleInstances

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances

Implies: `TypeSynonymInstances`

Allow definition of type class instances with arbitrary nested types in the instance head.

In Haskell 98 the head of an instance declaration must be of the form 
`C (T a1 ... an)`, where
- `C` is the class
- `T` is a data type constructor
- `a1 ... an` are distinct type variables

In the case of *multi-parameter type classes*, this rule applies to each parameter of the instance head.

GHC relaxes this rule in two ways:

1. With the `TypeSynonymInstances` extension, instance heads may use type synonyms. As always, using a type synonym is just shorthand for writing the RHS of the type synonym definition.

```hs
-- For example:
type Point a = (a,a)
instance C (Point a) where ...
-- is legal

-- The instance declaration is equivalent to
instance C (a,a) where ...

-- As always, type synonyms must be fully applied.
-- You cannot, for example, write:
instance Monad Point where ...
```

2. The `FlexibleInstances` extension allows the head of the instance declaration to mention arbitrary nested types.

For example, this becomes a legal instance declaration:
```hs
instance C (Maybe Int) where ...
```

See also the rules on overlap.

The FlexibleInstances extension implies TypeSynonymInstances.
